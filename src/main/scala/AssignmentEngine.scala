package auction
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.synthutils.PrintableParam

class AssignmentEngineParams(
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
  val priceRegStoreParams: RegStoreParams = new RegStoreParams(nPEs, 0, 1, agentWidth)
}

class WriteBackStream(ap: AssignmentEngineParams) extends Bundle {
  val start = Output(Bool())
  val wrData = Decoupled(UInt(64.W)) // TODO: One-size for prices AND agents? Or one streamwriter per?
  val finished = Input(Bool())
  val baseAddr = Output(UInt(32.W))
  val byteCount = Output(UInt(32.W))
}

class Assignment(private val ap: AssignmentEngineParams) extends Bundle {
  val agent = UInt(ap.agentWidth.W)
  val valid = Bool()
}

class AssignmentEngineIO(ap: AssignmentEngineParams) extends Bundle
{
  val searchTaskParams = new SearchTaskParams(
    bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs
  )

  val searchResultIn = Flipped(Decoupled(new SearchTaskResult(searchTaskParams)))

  // MemoryRqeust and memoryRequested are the two interfaces to the queues to Memory Controller
  val unassignedAgents = Decoupled(new AgentInfo(ap.bitWidth))
  val requestedAgents = Flipped(Decoupled(new AgentInfo(ap.bitWidth)))


  val rfInfo = new AppInfoSignals()

  val doWriteBack = Input(Bool())
  val writeBackDone = Output(Bool())

  val writeBackStream = new WriteBackStream(ap)

  // Interactions with the BramStore for prices
  val bramStoreReadAddr = Output(UInt(ap.agentWidth.W))
  val bramStoreReadData = Input(UInt(ap.bitWidth.W))

  val bramStoreWriteAddr = Output(UInt(ap.agentWidth.W))
  val bramStoreWriteData = Output(UInt(ap.bitWidth.W))
  val bramStoreWriteDataValid = Output(Bool())

  val bramStoreDump = Flipped(Decoupled(UInt(ap.bitWidth.W)))
  val bramStoreDumpStart = Output(Bool())


  def driveDefaults(): Unit = {
    searchResultIn.ready := false.B
    unassignedAgents.valid := false.B
    unassignedAgents.bits := DontCare
    requestedAgents.ready := false.B
    writeBackDone := false.B
    writeBackStream.start := false.B
    writeBackStream.wrData.valid := false.B
    writeBackStream.wrData.bits := DontCare
    writeBackStream.baseAddr := DontCare
    writeBackStream.byteCount := DontCare
    bramStoreReadAddr := DontCare
    bramStoreWriteAddr := DontCare
    bramStoreWriteData := DontCare
    bramStoreWriteDataValid := false.B

    bramStoreDump.ready := false.B
    bramStoreDumpStart := false.B
  }
}

class AssignmentEngine(ap: AssignmentEngineParams) extends Module {
  val io = IO(new AssignmentEngineIO(ap))
  io.driveDefaults()

  // regAssignments holds the mapping object->agent. regAssignment[2] == 3 => obj 2 is owned by agent 3
  val regAssignments = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U.asTypeOf(new Assignment(ap)))))

  // Stage 1
  val s1_agent = RegInit(0.U(ap.agentWidth.W))
  val s1_object = RegInit(0.U(ap.agentWidth.W))
  val s1_bid = RegInit(0.U(ap.bitWidth.W))
  val s1_currentPrice = RegInit(0.U(ap.bitWidth.W))
  val s1_valid = RegInit(false.B)


  val priceReadAddr = WireInit(0.U(ap.agentWidth.W))
  val priceReadRsp = Wire(UInt(ap.bitWidth.W))

  io.bramStoreReadAddr := priceReadAddr
  priceReadRsp := io.bramStoreReadData


  // State machine (so we can do the writeback stuff)
  val sIdle :: sWriteBackAssignments :: sWriteBackPrices1 :: sWriteBackPrices2 :: sWaitForFinished :: Nil = Enum(5)
  val regWBState = RegInit(sIdle)
  val regWBCount = RegInit(0.U(ap.agentWidth.W))
  val regWBPrice = RegInit(0.U(ap.bitWidth.W))

  val regNRows = RegInit(0.U(32.W))
  val regNCols = RegInit(0.U(32.W))
  val regBaseAddr = RegInit(0.U(32.W))
  val regByteCount = RegInit(0.U(32.W))

  val stall = !io.unassignedAgents.ready

  switch(regWBState) {
    is (sIdle) {
      when (io.rfInfo.start) {
        regNRows := io.rfInfo.nAgents
        regNCols := io.rfInfo.nObjects
        regBaseAddr := io.rfInfo.baseAddrRes
        regByteCount := io.rfInfo.nObjects * 16.U
      }

    when(!stall) {
      // Do IO
      io.searchResultIn.ready := true.B
      val fire = io.searchResultIn.fire()
      s1_valid := fire
      when(fire) {
        assert(io.requestedAgents.valid)
        io.requestedAgents.ready := true.B

        val searchRes = io.searchResultIn.bits
        priceReadAddr := searchRes.winner
        s1_currentPrice := priceReadRsp
        s1_object := searchRes.winner
        s1_bid := searchRes.bid
        s1_agent := io.requestedAgents.bits.agent
      }

      //  Update
      when(s1_valid) {
        when(s1_bid > s1_currentPrice) {
          // OK. Update everything

          // Kick out old guy
          io.unassignedAgents.valid := regAssignments(s1_object).valid
          io.unassignedAgents.bits.agent := regAssignments(s1_object).agent
          io.unassignedAgents.bits.nObjects := 0.U

          // Assign new
          regAssignments(s1_object).agent := s1_agent
          regAssignments(s1_object).valid := true.B

          io.bramStoreWriteData := s1_bid
          io.bramStoreWriteAddr := s1_object
          io.bramStoreWriteDataValid := true.B
        }.elsewhen(s1_bid > 0.U) {
          // Mis-speculation. Redo
          io.unassignedAgents.valid := true.B
          io.unassignedAgents.bits.agent := s1_agent
          io.unassignedAgents.bits.nObjects := 0.U
        }
      }
    }
  }
}



 switch (regWBState) {
    is (sIdle) {
      regWBCount := 0.U
      when (io.doWriteBack) {
        regWBState := sWriteBackAssignments
      }
    }
    is (sWriteBackAssignments)  {
      io.writeBackStream.start := true.B
      when(regWBCount === regNCols) {
        regWBCount := 0.U
        regWBState := sWriteBackPrices1
      }.otherwise {
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := regAssignments(regWBCount).agent
        io.writeBackStream.baseAddr := regBaseAddr
        io.writeBackStream.byteCount := regByteCount
        when(io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
    is (sWriteBackPrices1) {
      io.bramStoreDumpStart := true.B
      io.writeBackStream.start := true.B
      io.writeBackStream.wrData <> io.bramStoreDump

      when (io.bramStoreDump.fire()) {
       regWBCount := regWBCount + 1.U
        when (regWBCount === regNCols-1.U) {
          regWBCount := 0.U
          regWBState := sWaitForFinished

        }
      }

   }
   is (sWaitForFinished) {
      io.writeBackStream.start := true.B
      when (io.writeBackStream.finished === true.B) {
        regWBState := sIdle
        regAssignments.map(_ := 0.U.asTypeOf(new Assignment(ap)) )
        io.writeBackDone := true.B
      }
    }
  }
}
