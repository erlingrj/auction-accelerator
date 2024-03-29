package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.{MemReqParams, RoundUpAlign}
import fpgatidbits.ocm.{FPGAQueue, OCMMasterIF}

import fpgatidbits.synthutils.PrintableParam

// The controller monitors the Register file and sets up the initial queues for the problem
// It monitors the communication between Accountant and MemoryController and figures out when we are done
// It signals the Accountant to write all data to memory and then updates reg file with info that we are done

class MemCtrlParams(
  val bitWidth: Int,
  val nPEs: Int,
  val mrp: MemReqParams,
  val maxProblemSize: Int,
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
  def bramDataWidth: Int = (bitWidth + 1 + agentWidth)*nPEs // Enough to store data + col + last for each PE
  def bramAddrWidth: Int = maxProblemSize*maxProblemSize / (nPEs) //Enough to store max problemsize
  def bramAddrBits: Int = log2Ceil(maxProblemSize*maxProblemSize / (nPEs)) //Enough to store max problemsize
  def agentRowStoreParams: RegStoreParams = new RegStoreParams(1,1,0, agentWidth)
}

class AgentInfo(val agentWidth: Int) extends Bundle {
  val agent = UInt(agentWidth.W)
  val nObjects = UInt(agentWidth.W)
}


// Function to calculate how many bytes to request from DRAM based on nRows, nCols and bitwidth
object CalcNBytes {
  def apply(nRows: UInt, nCols: UInt, bitWidth: Int): UInt = {
    val elsPerWord = 64 / bitWidth //How many elements in one DRAM word (e.g. 8bit = 8 elements in 64 bit word)
    val elsPerRow = RoundUpAlign(elsPerWord, nCols) // How many elements we need per row. Aligned up to fill up entire words
    if (bitWidth == 8) {
      elsPerRow * nRows
    } else {
      elsPerRow * nRows * (bitWidth / 8).U
    }
  }
}
// Connects application to BRAM

//
class BramAgentInfo(val p: MemCtrlParams) extends Bundle {
  val idx = UInt(p.agentWidth.W)
  val addr = UInt(p.bramAddrWidth.W)
  val len = UInt(p.bramAddrWidth.W)
}

class BramElement(val bitWidth: Int, val agentWidth: Int) extends Bundle {
  val reward = UInt(bitWidth.W)
  val idx = UInt(agentWidth.W)
}

class BramMemWord(val nPEs: Int, val bitWidth: Int, val agentWidth: Int) extends Bundle {
  val els = Vec(nPEs, new BramElement(bitWidth, agentWidth))
  val valids = Vec(nPEs, Bool())
  val last = Bool()
}

class BramControllerIO(val p: MemCtrlParams) extends Bundle {
  // Input queue from Accountant
  val unassignedAgents = Flipped(Decoupled(new AgentInfo(p.agentWidth)))

  // Output queue to accountant
  val requestedAgents = Decoupled(new AgentInfo(p.agentWidth))

  // Request IO to the Agent row address RegStore
  val agentRowAddrReq = new RegStoreTransaction(new AgentRowInfo(p), p.agentRowStoreParams)

  // Interface to BRAM
  val bramReq = new OCMMasterIF(p.bramDataWidth, p.bramDataWidth, p.bramAddrBits)

  //  Control-outputs
  val dataDistOut = Decoupled(new BramMemWord(nPEs = p.nPEs, bitWidth = p.bitWidth, agentWidth = p.agentWidth))

  def driveDefaults: Unit = {
    unassignedAgents.ready := false.B
    requestedAgents.valid := false.B
    requestedAgents.bits := DontCare
    bramReq.req := DontCare
    bramReq.req.addr := 0.U
    bramReq.req.writeEn := false.B
    agentRowAddrReq.req.valid := false.B
    agentRowAddrReq.req.bits := DontCare
    agentRowAddrReq.rsp.ready:= false.B

    dataDistOut.valid := false.B
    dataDistOut.bits := DontCare
  }
}

// From accountant there is a steady stream of AgentInfo requests with
class BramController(val p: MemCtrlParams) extends MultiIOModule {
  val io = IO(new BramControllerIO(p))
  io.driveDefaults


  // Registers
  val regNumBramWordsLeft = RegInit(0.U(p.agentWidth.W))
  val regAgentRowAddr = RegInit(0.U(p.bramAddrBits.W))
  val regBramRspValid = RegInit(false.B)
  val regAgentReq = RegInit(0.U.asTypeOf(new AgentInfo(p.agentWidth)))
  regBramRspValid := false.B

  // Queue for memory responses
  val constBramRsps = 8
  val qBramRsps = Module(new FPGAQueue(new BramLine(p), constBramRsps)).io
  val qBramRspLast = Module(new FPGAQueue(Bool(), entries=constBramRsps+1)).io

  qBramRsps.enq.bits := io.bramReq.rsp.readData.asTypeOf(new BramLine(p))
  qBramRsps.enq.valid := regBramRspValid
  qBramRsps.deq.ready := false.B

  qBramRspLast.enq.bits := DontCare
  qBramRspLast.enq.valid := false.B
  qBramRspLast.deq.ready := false.B

  val regAgentRowInfo = RegInit(0.U.asTypeOf(new AgentRowInfo(p)))


  val sIdle ::sReq :: sReading :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  switch(regState) {
    is(sIdle) {
      when(qBramRsps.enq.ready &&
        qBramRsps.count < (constBramRsps - 3).U &&
        io.requestedAgents.ready
      ) {
        io.unassignedAgents.ready := true.B
        when(io.unassignedAgents.fire()) {
          val agentReq = io.unassignedAgents.bits
          regAgentReq := agentReq
          // Fetch address&length info from RegStore
          io.agentRowAddrReq.req.valid := true.B
          io.agentRowAddrReq.rsp.ready := true.B
          io.agentRowAddrReq.req.bits.addr := agentReq.agent
          regAgentRowInfo := io.agentRowAddrReq.rsp.bits.rdata.asTypeOf(new AgentRowInfo(p))

          regState := sReq

        }
      }
    }
    is (sReq) {

        io.requestedAgents.valid := true.B
        io.requestedAgents.bits := regAgentReq

          qBramRspLast.enq.valid := true.B
          qBramRspLast.enq.bits := regAgentRowInfo.length === 1.U

          when(regAgentRowInfo.length > 0.U) {

            regNumBramWordsLeft := regAgentRowInfo.length-1.U
            regAgentRowAddr := regAgentRowInfo.rowAddr

            // Make request to BRAM
            io.bramReq.req.writeEn := false.B
            io.bramReq.req.addr := regAgentRowInfo.rowAddr

            // Prepare the rsp queue for a Bram rsp next cycle
            regBramRspValid := true.B
          }.otherwise {
            regState := sIdle
            io.requestedAgents.valid := false.B
            qBramRspLast.enq.valid := false.B
          }
          when (regAgentRowInfo.length > 1.U) {
            regState := sReading
          }

          when (regAgentRowInfo.length === 1.U) {
            qBramRspLast.enq.bits := true.B
            regState := sIdle
          }

    }
    is(sReading) {
      // Fetch more data from BRAM
      when(io.requestedAgents.ready && qBramRsps.count < (constBramRsps-2).U) {

        io.bramReq.req.writeEn := false.B
        io.bramReq.req.addr := regAgentRowAddr + 1.U

        regBramRspValid := true.B

        regNumBramWordsLeft := regNumBramWordsLeft - 1.U
        regAgentRowAddr := regAgentRowAddr + 1.U

        qBramRspLast.enq.valid := true.B
        qBramRspLast.enq.bits := false.B

          when(regNumBramWordsLeft === 1.U) {
          qBramRspLast.enq.bits := true.B
//          io.requestedAgents.valid := true.B
//          io.requestedAgents.bits := regAgentReq
          regState := sIdle
        }
      }

    }
  }


  // Parse the data coming from BRAM
  when (io.dataDistOut.ready && io.requestedAgents.ready) {
    qBramRsps.deq.ready := true.B
    when(qBramRsps.deq.fire) {
      qBramRspLast.deq.ready := true.B

      // Parse data
      val bramLine = qBramRsps.deq.bits
      val bramWordOut = WireInit(0.U.asTypeOf(new BramMemWord(p.nPEs, p.bitWidth, p.agentWidth)))

      bramWordOut.els zip bramLine.els map { case (l, r) =>
        l.idx := r.col
        l.reward := r.value
      }
      bramWordOut.valids zip bramLine.els map { case (l, r) =>
        l := r.value > 0.U
      }
      bramWordOut.last := qBramRspLast.deq.bits

      // Send to data-dist
      io.dataDistOut.bits := bramWordOut
      io.dataDistOut.valid := true.B
    }
  }
}