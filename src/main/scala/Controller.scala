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

  val memoryRequestIn = Flipped(Decoupled(new AgentInfo(ap,mp)))
  val memoryRequestOut = Decoupled(new AgentInfo(ap,mp))

  val memoryRequestedIn = Flipped(Decoupled(new AgentInfo(ap,mp)))
  val memoryRequestedOut = Decoupled(new AgentInfo(ap,mp))

  val doWriteBack = Output(Bool())
  val writeBackDone = Input(Bool())

  def driveDefaults() = {
    memoryRequestedIn.ready := false.B
    memoryRequestedOut.valid := false.B
    memoryRequestedOut.bits := DontCare
    memoryRequestIn.ready := false.B
    memoryRequestOut.valid := false.B
    memoryRequestOut.bits := DontCare

    doWriteBack := false.B
    rfCtrl.finished := false.B
  }
}


class Controller(ap: AuctionParams, mp: MemReqParams) extends Module {
  val io = IO(new ControllerIO(ap, mp))
  io.driveDefaults()

  val sIdle :: sSetup :: sRunning :: sWriteBack :: sDone :: Nil = Enum(4)
  val regState = RegInit(sIdle)
  val regCount = RegInit(0.U(ap.agentWidth.W))

  // Connect memory requested to auction controller
  io.memoryRequestedIn <> io.memoryRequestedOut


  switch (regState) {
    is (sIdle) {
      // Wait for start signal
      when (io.rfInfo.start === true.B) {
        regState := sSetup
        regCount := io.rfInfo.nAgents
      }
    }
    is (sSetup) {
      when (regCount === 0.U) {
        regState := sRunning
      }.otherwise {
        io.memoryRequestOut.valid := true.B
        io.memoryRequestOut.bits.agent := regCount
        io.memoryRequestOut.bits.nObjects := io.rfInfo.nObjects
        regCount := regCount - 1.U
      }
    }
    is (sRunning) {
      // Here we connect the Accountant to the Memory Controller
      io.memoryRequestOut <> io.memoryRequestIn

      // Then we check for finished condition
      when (!io.memoryRequestedIn.ready && !io.memoryRequestIn.valid) {
        regState := sWriteBack
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
}
