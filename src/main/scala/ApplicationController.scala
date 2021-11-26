package auction
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.ocm.FPGAQueue
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

  val nElements = Output(UInt((ap.agentWidth*2).W))

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

  val sIdle :: sSetupBram1 :: sSetupBram2 :: sSetupQueues :: sRunning :: sWriteBack :: sDone :: Nil = Enum(7)

  val s2Idle :: s2SetupQueues :: s2Running :: s2Done :: Nil = Enum(4)


  val qUnassignedAgents = Module(new FPGAQueue(gen=new AgentInfo(ap.bitWidth), entries=ap.maxProblemSize-1))
  val qRequestedAgents = Module(new FPGAQueue(gen=new AgentInfo(ap.bitWidth), entries=ap.maxProblemSize-1))


  qUnassignedAgents.io := DontCare
  qUnassignedAgents.io.enq.valid := false.B
  qUnassignedAgents.io.deq.ready := false.B

  qRequestedAgents.io := DontCare
  qRequestedAgents.io.enq.valid := false.B
  qRequestedAgents.io.deq.ready := false.B


  io.requestedAgentsIn <> qRequestedAgents.io.enq


  val regState = RegInit(sIdle)
  val regState2 = RegInit(s2Idle)
  val regCount = RegInit(0.U(ap.agentWidth.W))

  val regBaseAddr = RegInit(0.U(32.W))
  val regNCols = RegInit(0.U(ap.agentWidth.W))
  val regNRows= RegInit(0.U(ap.agentWidth.W))
  val regNElements = RegInit(0.U((ap.agentWidth*2).W))


  regNElements := regNCols * regNRows
  io.nElements := regNElements

  switch (regState) {
    is (sIdle) {
      // Wait for start signal
      when (io.rfInfo.start === true.B) {
        regState := sSetupBram1
        regCount := io.rfInfo.nAgents - 1.U
        regBaseAddr := io.rfInfo.baseAddr
        regNRows := io.rfInfo.nAgents
        regNCols := io.rfInfo.nObjects
      }
    }
    is (sSetupBram1) {
      regState := sSetupBram2
    }
    is (sSetupBram2) {
      io.dram2bram_baseAddr := regBaseAddr
      io.dram2bram_nRows := regNRows
      io.dram2bram_nCols := regNCols
      io.dram2bram_start := true.B

      when (io.dram2bram_finished) {
        io.dram2bram_start := false.B
        regState := sRunning
      }
    }


    is (sRunning) {
      when (regState2 === s2Running) {
        // Then we check for finished condition
        io.requestedAgentsOut <> qRequestedAgents.io.deq
        io.unassignedAgentsOut <> qUnassignedAgents.io.deq
        io.unassignedAgentsIn <> qUnassignedAgents.io.enq
        when(RegNext(RegNext(qUnassignedAgents.io.count)) === 0.U && RegNext(qUnassignedAgents.io.count) ===0.U && qUnassignedAgents.io.count === 0.U &&  qRequestedAgents.io.count === 0.U && RegNext(qRequestedAgents.io.count) === 0.U) {
          regState := sWriteBack
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
        io.reinit := true.B
        regState := sIdle
        regState2 := s2Idle
      }
    }
  }


  switch(regState2) {
    is (s2Idle) {
      when (io.rfInfo.start) {
        regState2 := s2SetupQueues
      }
    }
    is (s2SetupQueues) {

        qUnassignedAgents.io.enq.valid := true.B
        qUnassignedAgents.io.enq.bits.agent := regCount
        qUnassignedAgents.io.enq.bits.nObjects := io.rfInfo.nObjects
        when (qUnassignedAgents.io.enq.fire()) {
          when (regCount === 0.U) {
            regState2 := s2Running
          }.otherwise {
            regCount := regCount - 1.U
          }
        }
    }
    is(s2Running) {
    }
  }

  val regCycleCount = RegInit(0.U(32.W))
  regCycleCount := regCycleCount + 1.U
  io.rfCtrl.cycleCount := regCycleCount
}
