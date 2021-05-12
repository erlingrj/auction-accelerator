package auction
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.synthutils.PrintableParam

class AccountantParams(
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

class WriteBackStream(ap: AccountantParams) extends Bundle {
  val start = Output(Bool())
  val wrData = Decoupled(UInt(64.W)) // TODO: One-size for prices AND agents? Or one streamwriter per?
  val finished = Input(Bool())
}

class Assignment(private val ap: AccountantParams) extends Bundle {
  val agent = UInt(ap.agentWidth.W)
  val valid = Bool()
}

class AccountantExtPriceIO(ap: AccountantParams) extends Bundle
{
  val searchTaskParams = new SearchTaskParams(
    bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs
  )

  val searchResultIn = Flipped(Decoupled(new SearchTaskResultPar(searchTaskParams)))

  // MemoryRqeust and memoryRequested are the two interfaces to the queues to Memory Controller
  val unassignedAgents = Decoupled(new AgentInfo(ap.bitWidth))
  val requestedAgents = Flipped(Decoupled(new AgentInfo(ap.bitWidth)))


  val rfInfo = new AppInfoSignals()

  val doWriteBack = Input(Bool())
  val writeBackDone = Output(Bool())

  val writeBackStream = new WriteBackStream(ap)

  val priceStoreS1 = new RegStoreTransaction(0.U(ap.bitWidth.W),ap.priceRegStoreParams)
  val priceStoreS2 = new RegStoreTransaction(0.U(ap.bitWidth.W),ap.priceRegStoreParams)

  def driveDefaults(): Unit = {
    searchResultIn.ready := false.B
    unassignedAgents.valid := false.B
    unassignedAgents.bits := DontCare
    requestedAgents.ready := false.B
    writeBackDone := false.B
    writeBackStream.start := false.B
    writeBackStream.wrData.valid := false.B
    writeBackStream.wrData.bits := DontCare
    priceStoreS2.req.valid := false.B
    priceStoreS2.req.bits := DontCare
    priceStoreS2.rsp.ready := false.B
    priceStoreS1.req.valid := false.B
    priceStoreS1.req.bits := DontCare
    priceStoreS1.rsp.ready := false.B
  }
}

class AccountantExtPricePipelined(ap: AccountantParams) extends Module
{
  val io = IO(new AccountantExtPriceIO(ap))
  io.driveDefaults()

  // regAssignments holds the mapping object->agent. regAssignment[2] == 3 => obj 2 is owned by agent 3
  val regAssignments = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U.asTypeOf(new Assignment(ap)))))

  // Stage 1
  val s1_agent = RegInit(0.U(ap.agentWidth.W))
  val s1_object = RegInit(0.U(ap.agentWidth.W))
  val s1_bid = RegInit(0.U(ap.bitWidth.W))
  val s1_currentPrice = RegInit(0.U(ap.bitWidth.W))
  val s1_valid = RegInit(false.B)


  val stall = !io.unassignedAgents.ready

  when (!stall) {
    // Do IO
    io.searchResultIn.ready := true.B
    val fire = io.searchResultIn.fire()
    s1_valid := fire
    when (fire) {
      assert(io.requestedAgents.valid)
      io.requestedAgents.ready := true.B

      val searchRes = io.searchResultIn.bits
      s1_currentPrice := io.priceStoreS1.read(searchRes.winner)
      s1_object := searchRes.winner
      s1_bid := searchRes.bid
      s1_agent := io.requestedAgents.bits.agent
    }

    //  Update
    when (s1_valid) {
      when(s1_bid > s1_currentPrice) {
        // OK. Update everything

        // Kick out old guy
        io.unassignedAgents.valid := regAssignments(s1_object).valid
        io.unassignedAgents.bits.agent := regAssignments(s1_object).agent
        io.unassignedAgents.bits.nObjects := 0.U

        // Assign new
        regAssignments(s1_object).agent := s1_agent
        regAssignments(s1_object).valid := true.B

        io.priceStoreS2.write(s1_bid, s1_object)
      }.otherwise {
        // Mis-speculation. Redo
        io.unassignedAgents.valid := true.B
        io.unassignedAgents.bits.agent := s1_agent
        io.unassignedAgents.bits.nObjects := 0.U
      }
    }
  }



  // State machine (so we can do the writeback stuff)
  val sIdle :: sWriteBackAssignments :: sWriteBackPrices :: sWaitForFinished :: Nil = Enum(4)
  val regWBState = RegInit(sIdle)
  val regWBCount = RegInit(0.U(ap.agentWidth.W))

 switch (regWBState) {
    is (sIdle) {
      regWBCount := 0.U
      when (io.doWriteBack) {
        regWBState := sWriteBackAssignments
      }
    }
    is (sWriteBackAssignments)  {
      io.writeBackStream.start := true.B
      when(regWBCount === io.rfInfo.nObjects) {
        regWBCount := 0.U
        regWBState := sWriteBackPrices
      }.otherwise {
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := regAssignments(regWBCount).agent
        when(io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
    is (sWriteBackPrices) {
      io.writeBackStream.start := true.B
      when (regWBCount === io.rfInfo.nObjects) {
        regWBCount := 0.U
        regWBState := sWaitForFinished
      }.otherwise {
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := io.priceStoreS1.read(regWBCount)
        when (io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
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
