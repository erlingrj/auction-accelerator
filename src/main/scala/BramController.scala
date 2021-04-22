package auction

import chisel3._
import chisel3.util._
import fpgatidbits.ocm.OCMMasterIF
import fpgatidbits.synthutils.PrintableParam



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
  val regAgentRowAddr = RegInit(0.U(p.agentWidth.W))
  val regBramRspValid = RegInit(false.B)
  val regAgentReq = RegInit(0.U.asTypeOf(new AgentInfo(p.agentWidth)))
  regBramRspValid := false.B

  // Queue for memory responses
  val qBramRsps = Module(new Queue(new BramLine(p), 8)).io
  qBramRsps.enq.bits := io.bramReq.rsp.readData.asTypeOf(new BramLine(p))
  qBramRsps.enq.valid := regBramRspValid
  qBramRsps.deq.ready := false.B

  val sIdle :: sReading :: Nil = Enum(2)
  val regState = RegInit(sIdle)

  switch (regState) {
    is (sIdle) {
      when (qBramRsps.enq.ready) {
        io.unassignedAgents.ready := true.B
        when (io.unassignedAgents.fire()) {
          val agentReq = io.unassignedAgents.bits
          regAgentReq := agentReq
          // Fetch address&length info from RegStore
          io.agentRowAddrReq.req.valid := true.B
          io.agentRowAddrReq.rsp.ready := true.B
          io.agentRowAddrReq.req.bits.addr := agentReq.agent
          val agentRowInfo = io.agentRowAddrReq.rsp.bits.rdata.asTypeOf(new AgentRowInfo(p))
          regNumBramWordsLeft := agentRowInfo.length
          regAgentRowAddr := agentRowInfo.rowAddr

          // Make request to BRAM
          io.bramReq.req.writeEn := false.B
          io.bramReq.req.addr := agentRowInfo.rowAddr

          // Prepare the rsp queue for a Bram rsp next cycle
          regBramRspValid := true.B
          regState := sReading
        }
        }
      }
    is (sReading) {
      when (io.dataDistOut.ready && io.requestedAgents.ready) {
        qBramRsps.deq.ready := true.B
        when(qBramRsps.deq.fire) {
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
          bramWordOut.last := regNumBramWordsLeft === 1.U

          // Send to data-dist
          io.dataDistOut.bits := bramWordOut
          io.dataDistOut.valid := true.B

          // Fetch more data from BRAM
          when(regNumBramWordsLeft > 1.U) {
            io.bramReq.req.writeEn := false.B
            io.bramReq.req.addr := regAgentRowAddr + 1.U

            regBramRspValid := true.B

            regNumBramWordsLeft := regNumBramWordsLeft - 1.U
            regAgentRowAddr := regAgentRowAddr + 1.U
          }.otherwise {
            io.requestedAgents.valid := true.B
            io.requestedAgents.bits := regAgentReq
            regState := sIdle
          }
        }
      }
    }
  }
}