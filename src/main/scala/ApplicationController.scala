package auction
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams

import fpgatidbits.synthutils.PrintableParam
import fpgatidbits.synthutils.PrintableParam
// The controller monitors the Register file and sets up the initial queues for the problem
// It monitors the communication between Accountant and MemoryController and figures out when we are done
// It signals the Accountant to write all data to memory and then updates reg file with info that we are done

class ApplicationControllerParams(
  val bitWidth: Int,
  val nPEs: Int,
  val mrp: MemReqParams,
  val maxProblemSize: Int
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
}



class ApplicationControllerIO(ap: ApplicationControllerParams) extends Bundle {

  val rfCtrl  = new AppControlSignals()
  val rfInfo = new AppInfoSignals()


  val dram2bram_start = Output(Bool())
  val dram2bram_finished = Input(Bool())
  val dram2bram_baseAddr = Output(UInt(64.W))
  val dram2bram_nRows = Output(UInt(ap.agentWidth.W))
  val dram2bram_nCols = Output(UInt(ap.agentWidth.W))

  val unassignedAgentsIn = Flipped(Decoupled(new AgentInfo(ap.bitWidth)))
  val unassignedAgentsOut = Decoupled(new AgentInfo(ap.bitWidth))

  val requestedAgentsIn = Flipped(Decoupled(new AgentInfo(ap.bitWidth)))
  val requestedAgentsOut = Decoupled(new AgentInfo(ap.bitWidth))

  val doWriteBack = Output(Bool())
  val writeBackDone = Input(Bool())
  val reinit = Output(Bool())

  def driveDefaults() = {
    requestedAgentsIn.ready := false.B
    requestedAgentsOut.valid := false.B
    requestedAgentsOut.bits := DontCare
    unassignedAgentsIn.ready := false.B
    unassignedAgentsOut.valid := false.B
    unassignedAgentsOut.bits := DontCare
    doWriteBack := false.B
    rfCtrl.finished := false.B

    reinit := false.B
    dram2bram_start := false.B
    dram2bram_nCols := 0.U
    dram2bram_nRows := 0.U
    dram2bram_baseAddr := 0.U
  }
}


class ApplicationController(ap: ApplicationControllerParams) extends Module {
  val io = IO(new ApplicationControllerIO(ap))
  io.driveDefaults()

  val constBackDownCount = 5

  val sIdle :: sSetupBram :: sSetupQueues :: sRunning :: sWriteBack :: sDone :: Nil = Enum(6)

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
        regState := sSetupBram
        regCount := io.rfInfo.nAgents - 1.U
      }
    }
    is (sSetupBram) {
      io.dram2bram_baseAddr := io.rfInfo.baseAddr
      io.dram2bram_nRows := io.rfInfo.nAgents
      io.dram2bram_nCols := io.rfInfo.nObjects
      io.dram2bram_start := true.B

      when (io.dram2bram_finished) {
        io.dram2bram_start := false.B
        regState := sSetupQueues
      }
    }

    is (sSetupQueues) {
      io.unassignedAgentsOut.valid := true.B
      io.unassignedAgentsOut.bits.agent := regCount
      io.unassignedAgentsOut.bits.nObjects := io.rfInfo.nObjects
      when (io.unassignedAgentsOut.fire()) {
        when (regCount === 0.U) {
          regState := sRunning
          regBackDownCount := constBackDownCount.U
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
      }.elsewhen(!io.requestedAgentsIn.valid && !io.unassignedAgentsIn.valid) {
          regBackDownCount := regBackDownCount - 1.U
        }.otherwise {
        regBackDownCount := constBackDownCount.U
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
        io.reinit := true.B
        regState := sIdle
      }
    }
  }

  val regCycleCount = RegInit(0.U(32.W))
  regCycleCount := regCycleCount + 1.U
  io.rfCtrl.cycleCount := regCycleCount
}
