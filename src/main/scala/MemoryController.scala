package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma._
import fpgatidbits.ocm.{FPGAQueue, OCMMasterIF}
import fpgatidbits.streams.ReadRespFilter
import fpgatidbits.synthutils.PrintableParam


// Function to calculate how many bytes to request from DRAM based on nRows, nCols and bitwidth
object CalcNBytes {
def apply(nRows: UInt, nCols: UInt, bitWidth: Int): UInt = {
  val elsPerWord = 64/bitWidth //How many elements in one DRAM word (e.g. 8bit = 8 elements in 64 bit word)
  val shift = log2Ceil(elsPerWord)
  val elsPerRow = RoundUpAlign(elsPerWord, nCols) // How many elements we need per row. Aligned up to fill up entire words

  if (bitWidth == 8) {
    elsPerRow * nRows
  } else {
    elsPerRow * nRows * (bitWidth/8).U
  }
}

}
class MemCtrlParams(
  val bitWidth: Int,
  val nPEs: Int,
  val mrp: MemReqParams,
  val maxProblemSize: Int,
  val bramDataWidth: Int = 512,
  val bramAddrWidth: Int = 8
) extends PrintableParam {
  override def headersAsList(): List[String] = {
    List(

    )
  }

  override def contentAsList(): List[String] = {
    List(

    )
  }
  def agentWidth = log2Ceil(maxProblemSize)
  def elBits = bitWidth + log2Ceil(maxProblemSize)
  def unusedBits = 72 - (elBits*nPEs) - 1
}


class MemoryIO(val mp: MemReqParams) extends Bundle {
  // I/O towards the memory
  val req = Decoupled(new GenericMemoryRequest(mp))
  val rsp = Flipped(Decoupled(new GenericMemoryResponse(mp)))

  def driveDefaults = {
    req.valid := false.B
    rsp.ready := false.B
    req.bits := DontCare
  }
}


class AgentInfo(val agentWidth: Int) extends Bundle {
  val agent = UInt(agentWidth.W)
  val nObjects = UInt(agentWidth.W)
}



class MemoryCtrlIO(ap: MemCtrlParams) extends Bundle {
  // Control-inputs this is where AuctionController adds the unassigned agents
  val unassignedAgents = Flipped(Decoupled(new AgentInfo(ap.agentWidth)))

  //  Control-outputs
  // memData goes to the datadistributor
  val ddP = new DataDistributorParams(
    bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs,
    memWidth = ap.mrp.dataWidth
  )

  val memData = Decoupled(new MemData(ddP))
  // RequestedAgents Goes back to AuctionController which dequeues one after the other
  val requestedAgents = Decoupled(new AgentInfo(ap.agentWidth))

  // appCtrl is connected to the regfil
  val regFile = new AppInfoSignals()

  def driveDefaults = {
    memData.valid := false.B
    memData.bits := 0.U.asTypeOf(new MemData(ddP))
    unassignedAgents.ready := false.B
    requestedAgents.valid := false.B
    requestedAgents.bits := DontCare
  }
}

// Abstract base class for the Auction Memory Controller
abstract class MemoryController(ap: MemCtrlParams) extends MultiIOModule {
  val ioMem = IO(new MemoryIO(ap.mrp))
  val ioCtrl = IO(new MemoryCtrlIO(ap))

  val constWordsPerRequst = ap.mrp.dataWidth / ap.bitWidth

  require(ap.bitWidth >= 8)
  val constBytesPerValue = (ap.bitWidth/8).U

  // This function generates a mask based on the numWordsLeft value
  def createOutputMask(numWordsLeft: UInt): UInt = {
    (1.U << numWordsLeft) - 1.U
  }

  // This function rounds up the number of bytes so we get alligned memory accesses
  //  This should probably be improved for performance
  def roundUpBytes(bytes: UInt): UInt = {
    val alignTo = ap.mrp.dataWidth/8 //we can only access individual bytes
    val alignToMask = (alignTo-1).U(ap.mrp.dataWidth.W)

    val result = WireInit(0.U(ap.mrp.dataWidth.W))

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
  ap: MemCtrlParams
                           ) extends MemoryController(ap)
{
  // drive defaults
  ioCtrl.driveDefaults
  ioMem.driveDefaults

  // read request generator
  val rg = Module(new ReadReqGen(ap.mrp, chanID=0, maxBeats=1))
  ioMem.req <> rg.io.reqs
  rg.io.ctrl.throttle := DontCare
  rg.io.ctrl.baseAddr := DontCare
  rg.io.ctrl.start := false.B
  rg.io.ctrl.byteCount := DontCare

  // Queue for memory responsens
  val qMemRsp = Module(new Queue(new GenericMemoryResponse(ap.mrp), 8)).io
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
          ioCtrl.memData.bits.mask := ~(0.U(ap.mrp.dataWidth.W))
          ioCtrl.memData.bits.last := false.B
          regNumWordsLeft := regNumWordsLeft - constWordsPerRequst.U
        }.elsewhen(regNumWordsLeft === constWordsPerRequst.U) {
          ioCtrl.memData.bits.mask := ~(0.U(ap.mrp.dataWidth.W))
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





class DRAMStream2BRAMStream(val p: MemCtrlParams) extends Module {
  val io = IO(new Bundle {
    val dramIn = Flipped(Decoupled(UInt(p.mrp.dataWidth.W)))
    val bramOut = Decoupled(UInt(p.bramDataWidth.W))
    val start = Input(Bool())
    val rowSize = Input(UInt(log2Ceil(p.maxProblemSize).W))
  })

  val regBRAMWord = UInt(p.bramDataWidth.W)

}


