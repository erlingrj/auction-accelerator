package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma._
import fpgatidbits.streams.ReadRespFilter


class MemoryIO(private val ap: AuctionParams, private val mp: MemReqParams) extends Bundle {
  // I/O towards the memory
  val req = Decoupled(new GenericMemoryRequest(mp))
  val rsp = Flipped(Decoupled(new GenericMemoryResponse(mp)))

  def driveDefaults = {
    req.valid := false.B
    rsp.ready := false.B
    req.bits := DontCare
  }
}

class AgentInfo(private val ap: AuctionParams, private val mp: MemReqParams) extends Bundle {
  val agent = UInt(ap.agentWidth.W)
  val nObjects = UInt(ap.agentWidth.W)
}

class MemoryCtrlIO(ap: AuctionParams, mp: MemReqParams) extends Bundle {
  // Control-inputs this is where AuctionController adds the unassigned agents
  val unassignedAgents = Flipped(Decoupled(new AgentInfo(ap,mp)))

  //  Control-outputs
  // memData goes to the datadistributor
  val memData = Decoupled(new MemData(ap))
  // RequestedAgents Goes back to AuctionController which dequeues one after the other
  val requestedAgents = Decoupled(new AgentInfo(ap,mp))

  // appCtrl is connected to the regfil
  val regFile = new AppInfoSignals()

  def driveDefaults = {
    memData.valid := false.B
    memData.bits := 0.U.asTypeOf(new MemData(ap))
    unassignedAgents.ready := false.B
    requestedAgents.valid := false.B
    requestedAgents.bits := DontCare
  }
}

// Abstract base class for the Auction Memory Controller
abstract class MemoryController(ap: AuctionParams,mp: MemReqParams) extends MultiIOModule {
  val ioMem = IO(new MemoryIO(ap,mp))
  val ioCtrl = IO(new MemoryCtrlIO(ap,mp))

  val constWordsPerRequst = mp.dataWidth / ap.bitWidth

  require(ap.bitWidth >= 8)
  val constBytesPerValue = (ap.bitWidth/8).U

  // This function generates a mask based on the numWordsLeft value
  def createOutputMask(numWordsLeft: UInt): UInt = {
    (1.U << numWordsLeft) - 1.U
  }

  // This function rounds up the number of bytes so we get alligned memory accesses
  //  This should probably be improved for performance
  def roundUpBytes(bytes: UInt): UInt = {
    val alignTo = mp.dataWidth/8 //we can only access individual bytes
    val alignToMask = (alignTo-1).U(mp.dataWidth.W)

    val result = WireInit(0.U(mp.dataWidth.W))

    when((bytes & alignToMask) === 0.U) {
      result := bytes
    }.otherwise {
      val temp = WireInit(bytes & (~alignToMask).asUInt)
      result := temp + alignTo.U
    }
    result
  }

  // Get the Base address for the price row belonging to an object
  // remember that all addresses are aligned to 8-byte boundaries

  def getBaseAddrForAgent(agent: UInt, nObjs: UInt, baseAddr: UInt) : UInt = {
    val nObjsWireExt = Wire(UInt((nObjs.getWidth+1).W)) // Extend nObjs with 1 bit so we can fit rowsPerAgent on itg
    nObjsWireExt := nObjs
    val rowsPerAgent = RoundUpAlign(8, nObjsWireExt*constBytesPerValue)
    baseAddr + (agent*rowsPerAgent)
  }
}


// First iteration of memory controller. Just using the DRAM on the Zynq
class AuctionDRAMController(
                           ap: AuctionParams,
                           mp: MemReqParams
                           ) extends MemoryController(ap,mp)
{
  // drive defaults
  ioCtrl.driveDefaults
  ioMem.driveDefaults

  // read request generator
  val rg = Module(new ReadReqGen(mp, chanID=0, maxBeats=1))
  ioMem.req <> rg.io.reqs
  rg.io.ctrl.throttle := DontCare
  rg.io.ctrl.baseAddr := DontCare
  rg.io.ctrl.start := false.B
  rg.io.ctrl.byteCount := DontCare

  // Queue for memory responsens
  val qMemRsp = Module(new Queue(new GenericMemoryResponse(mp), 8)).io
  qMemRsp.enq <> ioMem.rsp
  qMemRsp.deq.ready := false.B // INitialize to false
  // No errors
  assert(rg.io.stat.error === false.B, "[mem-ctrl] error in RG")

  // State machine
  val sIdle :: sReading :: Nil = Enum(2)
  val regState = RegInit(sIdle)
  val regNumWordsLeft = RegInit(0.U)

  switch(regState) {
    is(sIdle) {
      ioCtrl.unassignedAgents.ready := true.B //Accept new requests from AuctionController
      when (ioCtrl.unassignedAgents.fire === true.B) {
        // Start the read request generator
        val agentReq = ioCtrl.unassignedAgents.bits
        regNumWordsLeft := agentReq.nObjects
        rg.io.ctrl.start := true.B
        rg.io.ctrl.throttle := false.B
        rg.io.ctrl.baseAddr := getBaseAddrForAgent(agentReq.agent, agentReq.nObjects, ioCtrl.regFile.baseAddr)
        rg.io.ctrl.byteCount := roundUpBytes((agentReq.nObjects * ap.bitWidth.U)/8.U)
        regState := sReading

        // Add that agent to the requested queue
        ioCtrl.requestedAgents.valid := true.B
        ioCtrl.requestedAgents.bits := agentReq
        assert(ioCtrl.requestedAgents.ready === true.B)
      }
    }

    is(sReading) {
      // Connect the data distributor to the rsp port
      qMemRsp.deq.ready := ioCtrl.memData.ready
      when (qMemRsp.deq.fire()) {

        ioCtrl.memData.valid := true.B
        ioCtrl.memData.bits.data := qMemRsp.deq.bits.readData

        when (regNumWordsLeft > constWordsPerRequst.U) {
          ioCtrl.memData.bits.mask := ~(0.U(ap.memWidth.W))
          ioCtrl.memData.bits.last := false.B
          regNumWordsLeft := regNumWordsLeft - constWordsPerRequst.U
        }.elsewhen(regNumWordsLeft === constWordsPerRequst.U) {
          ioCtrl.memData.bits.mask := ~(0.U(ap.memWidth.W))
          ioCtrl.memData.bits.last := true.B
          regNumWordsLeft := 0.U
          regState := sIdle
        }.otherwise {
          ioCtrl.memData.bits.mask := createOutputMask(regNumWordsLeft)
          ioCtrl.memData.bits.last := true.B
          regNumWordsLeft := 0.U
          regState := sIdle
          }
        }
    }
  }
}









