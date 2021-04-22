package auction
import chisel3._
import chisel3.util._

class AccountantExtPriceIO(ap: AccountantParams) extends Bundle
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

  val priceStore = new RegStoreTransaction(0.U(ap.bitWidth.W),ap.priceRegStoreParams)

  val notifyPEsContinue = Output(Bool())

  def driveDefaults(): Unit = {
    searchResultIn.ready := false.B
    unassignedAgents.valid := false.B
    unassignedAgents.bits := DontCare
    requestedAgents.ready := false.B
    writeBackDone := false.B
    writeBackStream.start := false.B
    writeBackStream.wrData.valid := false.B
    writeBackStream.wrData.bits := DontCare
    priceStore.req.valid := false.B
    priceStore.req.bits := DontCare
    priceStore.rsp.ready := false.B
    notifyPEsContinue := false.B
  }
}

class AccountantExtPriceNonPipelined(ap: AccountantParams) extends Module
{
  val io = IO(new AccountantExtPriceIO(ap))
  io.driveDefaults()

  // regAssignments holds the mapping object->agent. regAssignment[2] == 3 => obj 2 is owned by agent 3
  val regAssignments = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U.asTypeOf(new Assignment(ap)))))
  val regCurrentAgent = RegInit(0.U)

  val regObject = RegInit(0.U(ap.agentWidth.W))
  val regBid = RegInit(0.U(ap.bitWidth.W))
  val regCurrentPrice = RegInit(0.U(ap.bitWidth.W))
  // State machine

  val sWaitForBid :: sUpdate :: sWriteBackAssignments :: sWriteBackPrices :: sWaitForFinished :: Nil = Enum(5)
  val regState = RegInit(sWaitForBid)
  val regInitCount = 0.U(ap.agentWidth.W)
  val regWBCount = RegInit(0.U(ap.maxProblemSize.W))

  switch (regState) {
    is (sWaitForBid) {
      // Wait for searchResultIn and MemoryRequested
      io.searchResultIn.ready := true.B
      when (io.searchResultIn.fire()) {
        io.requestedAgents.ready := true.B
        assert(io.requestedAgents.valid === true.B)
        regObject := io.searchResultIn.bits.winner
        regBid := io.searchResultIn.bits.bid
        regCurrentAgent := io.requestedAgents.bits.agent

        // Fetch current price
        regCurrentPrice := io.priceStore.read(io.searchResultIn.bits.winner)

        regState := sUpdate
      }

      when(io.doWriteBack) {
        regState := sWriteBackAssignments
      }
    }

    is (sUpdate) {
      val newRequest = WireInit(false.B)
      when (regBid > 0.U) {
        // We have a bid
        // Kick out old guy
        io.unassignedAgents.valid := regAssignments(regObject).valid
        newRequest := regAssignments(regObject).valid
        io.unassignedAgents.bits.agent := regAssignments(regObject).agent
        io.unassignedAgents.bits.nObjects := io.rfInfo.nObjects

        // Assign new
        regAssignments(regObject).agent := regCurrentAgent
        regAssignments(regObject).valid := true.B

        io.priceStore.write(regCurrentPrice + regBid, regObject)

        // Check if we were able to fire off new memory request (or maybe we didnt have to)
        when(io.unassignedAgents.fire || !newRequest) {
          regState := sWaitForBid
          io.notifyPEsContinue := true.B
        }.otherwise { // If we werent able to fire the memory request. Redo the update stage
          regState := sUpdate
        }
      }.otherwise { // If the bid is 0 we drop this agent since there are no valid bids for him
        regState := sWaitForBid
      }
    }
    is (sWriteBackAssignments)  {
      io.writeBackStream.start := true.B
      when(regWBCount === io.rfInfo.nObjects) {
        regWBCount := 0.U
        regState := sWriteBackPrices
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
        regState := sWaitForFinished
      }.otherwise {
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := io.priceStore.read(regWBCount)
        when (io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
    is (sWaitForFinished) {
      io.writeBackStream.start := true.B
      when (io.writeBackStream.finished === true.B) {
        regState := sWaitForBid
        regAssignments.map(_ := 0.U.asTypeOf(new Assignment(ap)) )
        io.writeBackDone := true.B
      }
    }
  }
}
