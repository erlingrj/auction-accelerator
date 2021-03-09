package auction
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams

// The controller monitors the Register file and sets up the initial queues for the problem
// It monitors the communication between Accountant and MemoryController and figures out when we are done
// It signals the Accountant to write all data to memory and then updates reg file with info that we are done



class ControllerIO(ap: AuctionParams, mp: MemReqParams) extends Bundle {

  val rfCtrl  = new AppControlSignals()
  val rfInfo = new AppInfoSignals()

  val unassignedAgentsIn = Flipped(Decoupled(new AgentInfo(ap,mp)))
  val unassignedAgentsOut = Decoupled(new AgentInfo(ap,mp))

  val requestedAgentsIn = Flipped(Decoupled(new AgentInfo(ap,mp)))
  val requestedAgentsOut = Decoupled(new AgentInfo(ap,mp))

  val doWriteBack = Output(Bool())
  val writeBackDone = Input(Bool())

  def driveDefaults() = {
    requestedAgentsIn.ready := false.B
    requestedAgentsOut.valid := false.B
    requestedAgentsOut.bits := DontCare
    unassignedAgentsIn.ready := false.B
    unassignedAgentsOut.valid := false.B
    unassignedAgentsOut.bits := DontCare

    doWriteBack := false.B
    rfCtrl.finished := false.B
  }
}


class Controller(ap: AuctionParams, mp: MemReqParams) extends Module {
  val io = IO(new ControllerIO(ap, mp))
  io.driveDefaults()

  val constBackDownCount = 10

  val sIdle :: sSetup :: sRunning :: sWriteBack :: sDone :: Nil = Enum(5)
  val regState = RegInit(sIdle)
  val regCount = RegInit(0.U(ap.agentWidth.W))
  val regBackDownCount = RegInit(0.U(log2Ceil(constBackDownCount).W))

  // Connect memory requested to auction controller
  io.requestedAgentsIn <> io.requestedAgentsOut

  when (io.requestedAgentsOut.fire()) {
    regBackDownCount := constBackDownCount.U
  }

  switch (regState) {
    is (sIdle) {
      // Wait for start signal
      when (io.rfInfo.start === true.B) {
        regState := sSetup
        regCount := io.rfInfo.nAgents - 1.U
      }
    }
    is (sSetup) {
        io.unassignedAgentsOut.valid := true.B
        io.unassignedAgentsOut.bits.agent := regCount
        io.unassignedAgentsOut.bits.nObjects := io.rfInfo.nObjects
        when (io.unassignedAgentsOut.fire()) {
          when (regCount === 0.U) {
            regState := sRunning
          }.otherwise {
            regCount := regCount - 1.U
          }
        }
    }

    is (sRunning) {
      // Here we connect the Accountant to the Memory Controller
      io.unassignedAgentsOut <> io.unassignedAgentsIn

      // Then we check for finished condition
      when (!io.requestedAgentsIn.valid && !io.unassignedAgentsIn.valid && regBackDownCount === 0.U) {
        regState := sWriteBack
      }.otherwise {
        when (regBackDownCount > 0.U) {
          regBackDownCount := regBackDownCount - 1.U
        }
      }
    }
    is (sWriteBack) {
      io.doWriteBack := true.B
      when (io.writeBackDone) {
        regState := sDone
      }
    }
    is (sDone) {
      io.rfCtrl.finished := true.B
      when (io.rfInfo.start) {
        regState := sSetup
      }
    }
  }

  val regCycleCount = RegInit(0.U(32.W))
  regCycleCount := regCycleCount + 1.U
  io.rfCtrl.cycleCount := regCycleCount
}
