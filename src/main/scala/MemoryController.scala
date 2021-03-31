package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma._
import fpgatidbits.ocm.{FPGAQueue, OCMMasterIF}
import fpgatidbits.streams.ReadRespFilter
import fpgatidbits.synthutils.PrintableParam


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



class BramLine(val mp: MemCtrlParams) extends Bundle {
  val els = Vec(mp.nPEs, new BramEl(mp))
  val last = Bool()
  val unused = UInt(mp.unusedBits.W)
}


class BramEl(val mp: MemCtrlParams) extends Bundle {
  val value = UInt(mp.bitWidth.W)
  val col = UInt(log2Ceil(mp.maxProblemSize).W)
  val last = Bool()
}

class AgentRowInfo(val agentBits: Int, val bramDataBits: Int) extends Bundle {
  val agentId = UInt(agentBits.W)
  val valid = Bool()
  val rowAddr = UInt(bramDataBits.W)
  val colAddr = UInt(bramDataBits.W)
}

class DRAM2BRAMIO(val p: MemCtrlParams) extends Bundle {
  // Interface to DRAM
  val dramReq = Decoupled(new GenericMemoryRequest(p.mrp))
  val dramRsp = Flipped(Decoupled(new GenericMemoryResponse(p.mrp)))

  // Interface to BRAM
  val bramCmd = new OCMMasterIF(p.bramDataWidth, p.bramDataWidth, p.bramAddrWidth)

  // Control interface
  val start = Input(Bool())
  val finished = Output(Bool())
  val baseAddr = Input(UInt(64.W))
  val nRows = Input(UInt(log2Ceil(p.maxProblemSize).W))
  val nCols = Input(UInt(log2Ceil(p.maxProblemSize).W))

  // Interface to module storing the agentRowAddresses
  // We need nPEs because worst case each memory fetch leads to nPE rows.
  val agentRowAddress = Decoupled(new AgentRowInfo(log2Ceil(p.maxProblemSize), p.bramDataWidth))

  def driveDefaults() = {
    dramReq.valid := false.B
    dramReq.bits := DontCare
    dramRsp.ready := false.B
    bramCmd := DontCare
    bramCmd.req.writeEn := false.B
    agentRowAddress.bits := DontCare
    agentRowAddress.valid := false.B
  }
}

class DRAM2BRAM(val p: MemCtrlParams) extends Module {
  val io = new DRAM2BRAMIO(p)

  require(p.bitWidth >= 8)
  require(p.nPEs >= 8)
  // read request generator
  val rg = Module(new ReadReqGen(p.mrp, chanID=0, maxBeats=1))
  io.dramReq <> rg.io.reqs
  rg.io.ctrl.throttle := DontCare
  rg.io.ctrl.baseAddr := DontCare
  rg.io.ctrl.byteCount := DontCare

  // FIFO for reciving DRAM memory data
  val rspQ = Module(new FPGAQueue(new GenericMemoryResponse(p.mrp), 64/p.bitWidth)).io
  io.dramRsp <> rspQ.enq
  rspQ.deq.ready := false.B

  val regBytesLeftInRow = RegInit(0.U(32.W))

  // Messy function that does several things:
  // 1. Splits DRAM Line into BramEls vector (also masks out unused bits due to 64 bit alignment of rows)
  // 2. Also deals with the global regBytesLeftInRow register
  def dramRowToBramEls(dramRow: UInt): Vec[BramEl] = {
    val bramEls = WireInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
      bramEls.zipWithIndex.map { case (b, i) =>
        when(regBytesLeftInRow > i.U) {
          b.value := dramRow((i + 1) * p.bitWidth - 1, i * p.bitWidth)
        }
      }
    when(regBytesLeftInRow > (64/p.bitWidth).U) {
      regBytesLeftInRow := regBytesLeftInRow - (64/p.bitWidth).U // Doenst matter if it overflows the last round
    }.otherwise {
      regBytesLeftInRow := regNBytesInRow
      regCurrentRowHasValid := false.B
    }


    bramEls
  }

  val sIdle :: sRunning :: sRunningLeftOvers :: Nil = Enum(3)
  val regState = RegInit(sIdle)
  val regBramLine = RegInit(new BramLine(p))
  val regBramElsLeftOvers = RegInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))

  val regElCnt = RegInit(0.U(log2Ceil(p.nPEs)))
  val regRowAddrCnt = RegInit(0.U(p.bramAddrWidth.W))
  val regColAddrCnt = RegInit(0.U(p.bramAddrWidth.W))
  val regAgentRowCnt = RegInit(0.U(p.agentWidth))
  val regBramRowCnt = RegInit(0.U(p.bramAddrWidth.W))

  val regNRows = RegInit(0.U(log2Ceil(p.agentWidth)))
  val regNCols = RegInit(0.U(log2Ceil(p.agentWidth)))
  val regNBytesInRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowsPerMatrixRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowIdx = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))

  val regCurrentRowHasValid = RegInit(false.B)



  // State machine:

  // After reciving a word from memory we have to:
  // 1. Split it up into n elements
  // 2. Create BramEls out of non-zero elements
  // 3. Add them to the BramLine which builds up over multiple iterations
  // There are a couple of scenarios
  // A. Some of the BramEl represents the first element in a row =>
  //  We add that information to the appropriate element of the RowAddr
  // B. This mem-row fills up a bram-line
  //  We fire of the bram-line and next cycle we finish with the left-overs (not accepting new data from mem)
  switch (regState) {
    is (sIdle) {
      rg.io.ctrl.start := false.B
      io.finished := false.B
      when (io.start) {
        rg.io.ctrl.throttle := false.B
        rg.io.ctrl.baseAddr := io.baseAddr
        val byteCount = io.nRows * io.nCols * (p.bitWidth/8).U
        rg.io.ctrl.byteCount := byteCount
        val bytesInRow = (io.nCols >> (p.bitWidth>>3))
        regNBytesInRow := bytesInRow
        regBytesLeftInRow := bytesInRow

        regState := sRunning

        regNCols := io.nCols
        regNRows := io.nRows
        regDramRowIdx := 0.U
        regElCnt := 0.U
        regBramLine := 0.U
      }
    }
    is (sRunning) {
      rspQ.deq.ready := true.B
      when (rspQ.deq.fire()) {
        val bramEls = dramRowToBramEls(rspQ.deq.bits.readData)
        bramEls.zipWithIndex.map{case (b,i) =>
          val col = WrapAdd(regColAddrCnt,i.U, regNCols)
          b.col := col
          b.last := col === regNCols
        }

        val nValids = PopCount(bramEls.map(_.value =/= 0.U))
        val valids = Compactor(bramEls, bramEls.map(_.value =/= 0.U))

        // First check whether we have any new rows starting this round
        when(!regCurrentRowHasValid) {
          when(nValids > 0.U) {
            io.agentRowAddress.ready := true.B
            io.agentRowAddress.bits.agentId := regAgentRowCnt
            io.agentRowAddress.bits.colAddr := regElCnt
            io.agentRowAddress.bits.rowAddr := regBramRowCnt
            regCurrentRowHasValid := true.B
          }
        }

        val newRegElCnt = Cat(0.U(1.W), regElCnt) + nValids
        when (newRegElCnt >= p.nPEs.U) {
          // We are filling up a bramLine. Fire it of and fit the remainder on the next line
          // 1. fill the remaining spots
          val bramLine = WireInit(0.U.asTypeOf(new BramLine(p)))
          val spotsLeft = p.nPEs.U - regElCnt
          bramLine.els.zipWithIndex.map { case (el, i) =>
            when(i.U < regElCnt) {
              el := regBramLine.els(i)
            }.otherwise{
              el := valids(i.U-regElCnt)
            }
          }
          // Fire it off
          io.bramCmd.req.writeEn := true.B
          io.bramCmd.req.writeData := bramLine

          // Add the remaining to the regBramLine
          valids.zipWithIndex.map{case (el,i) =>
            when(i.U>=spotsLeft) {
              regBramLine.els(i.U-spotsLeft) := el
            }
          }
          // Increment the idx
          regElCnt := regElCnt + (p.nPEs.U - spotsLeft)

        }.otherwise{
          // Add the valid guys to the bramLine and increment
          valids.zipWithIndex.map{case (el, i) =>
            when(i.U < nValids) {
              regBramLine.els(regElCnt + i.U) := el
            }
          }
          // Increment the idx
          regElCnt := newRegElCnt
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


