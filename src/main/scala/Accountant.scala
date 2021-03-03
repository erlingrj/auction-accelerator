package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams

// THe accountant keeps the books on assignments and prices and gives the prices to the PEs


abstract class Accountant(ap: AuctionParams, mp: MemReqParams) extends MultiIOModule {
  val io = IO(new AccountantIO(ap,mp))

  def getStartAddrForRow(rowIdx: UInt, nCols: UInt, baseAddr: UInt): UInt = {
    baseAddr + rowIdx*nCols*(ap.bitWidth/8).U
  }
  io.driveDefaults()
}


class WriteBackStream(ap: AuctionParams, mp:MemReqParams) extends Bundle {
  val start = Output(Bool())
  val wrData = Decoupled(UInt(32.W)) // TODO: One-size for prices AND agents? Or one streamwriter per?
}

class AccountantIO(ap: AuctionParams,mp: MemReqParams) extends Bundle {
  val searchResultIn = Flipped(Decoupled(new SearchTaskResult(ap)))

  // MemoryRqeust and memoryRequested are the two interfaces to the queues to Memory Controller
  val memoryRequest = Decoupled(new AgentInfo(ap,mp))
  val memoryRequested = Flipped(Decoupled(new AgentInfo(ap,mp)))

  val PEControlOut = Vec(ap.nPEs, Decoupled(new PEControl(ap)))

  val rfInfo = new AppInfoSignals()

  val doWriteBack = Input(Bool())
  val writeBackDone = Output(Bool())

  val writeBackStream = new WriteBackStream(ap,mp)

  def driveDefaults(): Unit = {
    PEControlOut.map({case (out) =>
      out.valid := false.B
      out.bits := DontCare
    })
    searchResultIn.ready := false.B
    memoryRequest.valid := false.B
    memoryRequest.bits := DontCare
    memoryRequested.ready := false.B
    writeBackDone := false.B
    writeBackStream.start := false.B
    writeBackStream.wrData.valid := false.B
    writeBackStream.wrData.bits := DontCare
  }
}

class Assignment(private val ap: AuctionParams) extends Bundle {
  val agent = UInt(ap.agentWidth.W)
  val valid = Bool()
}

// First try on the parallel auction controller. A non-pipelined version
class AccountantNonPipelined(ap: AuctionParams, mp: MemReqParams)
  extends Accountant(ap,mp)
{
  // regAssignments holds the mapping object->agent. regAssignment[2] == 3 => obj 2 is owned by agent 3
  val regAssignments = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U.asTypeOf(new Assignment(ap)))))
  val regCurrentAgent = RegInit(0.U)
  val regPrices = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U(ap.bitWidth.W))))

  // Connect prices to output
  var regPriceIdx = 0
  io.PEControlOut.map(_.valid := true.B)
  regPrices.zipWithIndex.map { case (p: UInt, i: Int) =>
    io.PEControlOut((i % ap.nPEs)).bits.prices(i / ap.nPEs) := p
  }

  val regObject = RegInit(0.U(ap.agentWidth.W))
  val regBid = RegInit(0.U(ap.bitWidth.W))

  // State machine

  val sWaitForBid :: sUpdate :: sWriteBackAssignments :: sWriteBackPrices :: Nil = Enum(4)
  val regState = RegInit(sWaitForBid)
  val regInitCount = 0.U(ap.agentWidth.W)
  val regWBCount = RegInit(0.U(ap.maxProblemSize.W))

  switch (regState) {
    is (sWaitForBid) {
      // Wait for searchResultIn and MemoryRequested
      io.searchResultIn.ready := true.B
      when (io.searchResultIn.fire()) {
        io.memoryRequested.ready := true.B
        assert(io.memoryRequested.valid === true.B)
        regObject := io.searchResultIn.bits.winner
        regBid := io.searchResultIn.bits.bid
        regCurrentAgent := io.memoryRequested.bits.agent
        regState := sUpdate
      }

      when(io.doWriteBack) {
        regState := sWriteBackAssignments
      }
    }

    is (sUpdate) {
      val newRequest = WireInit(false.B)
      when (regBid > 0.U) {
        // We have a valid bid but we have to check whether the bid already theres is higher
        when(regAssignments(regObject).valid && (regPrices(regObject) > regBid))  {
          io.memoryRequest.valid := true.B
          newRequest := true.B
          io.memoryRequest.bits.agent := regCurrentAgent
          io.memoryRequest.bits.nObjects := io.rfInfo.nObjects
          assert(io.memoryRequest.ready === true.B)
        }.otherwise {
          // Kick out the potentially old guy
          io.memoryRequest.valid := regAssignments(regObject).valid
          newRequest := regAssignments(regObject).valid
          io.memoryRequest.bits.agent := regAssignments(regObject).agent
          io.memoryRequest.bits.nObjects := io.rfInfo.nObjects
          assert(io.memoryRequest.ready === true.B)

          // Update assignments and prices
          regAssignments(regObject).agent:= regCurrentAgent
          regAssignments(regObject).valid := true.B

          regPrices(regObject) := regBid
        }
      }
      // Check if we were able to fire off new memory request (or maybe we didnt have to)
      when(io.memoryRequest.fire || !newRequest) {
          regState := sWaitForBid
        }.otherwise { // If we werent able to fire the memory request. Redo the update stage
          regState := sUpdate
        }
      }
    is (sWriteBackAssignments)  {
      when(regWBCount === io.rfInfo.nObjects) {
        regWBCount := 0.U
        regState := sWriteBackPrices
      }.otherwise {
        io.writeBackStream.start := true.B
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := regAssignments(regWBCount).agent
        when(io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
    is (sWriteBackPrices) {
      when (regWBCount === io.rfInfo.nObjects) {
        regWBCount := 0.U
        regState := sWaitForBid
        io.writeBackDone := true.B
      }.otherwise {
        io.writeBackStream.start := true.B
        io.writeBackStream.wrData.valid := true.B
        io.writeBackStream.wrData.bits := regPrices(regWBCount)
        when (io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
  }
}


/*
 class AuctionControllerPar(ap: AuctionParams) extends MultiIOModule {
   val io = IO(new AuctionControllerParIO(ap))
   io.driveDefaults

   // getRowBaseAddr returns the start-address for a given row.
   def getRowBaseAddr(rowIdx: UInt, nCols: UInt, baseAddr: UInt): UInt = {
     baseAddr + rowIdx*nCols*(ap.bitWidth/8).U
   }

   val constUnassigned = ap.nPEs
   val constAssSz = log2Ceil(ap.nPEs)
   // This function looks up object at index <obj> and checks if the MSb is set
   def objectIsAssigned(obj: UInt): Bool = {
     (regAssignments(obj) >> constAssSz).asUInt === 0.U
   }

   // Initialize the agent->object assignments. Unassigned == high bit set
   val regAssignments = RegInit(VecInit(Seq.fill(ap.nPEs){
     0.U((log2Ceil(ap.nPEs) + 1).W)}))
   val qUnassigned = Module(new Queue(UInt(), ap.nPEs)).io
   qUnassigned.deq.ready := false.B
   qUnassigned.enq.valid := false.B
   qUnassigned.enq.bits := DontCare

   val regCurrentAgent= RegInit(0.U)

   val regPrices = RegInit(VecInit(Seq.fill(ap.nPEs){0.U(ap.bitWidth.W)}))
   // Connect prices to the PEs
   (io.pricesOut zip regPrices).map({
     case (io, reg) =>
       io.valid := true.B
       io.bits := reg
   })



   val sSetup :: sIdle :: sReading :: sFinished :: Nil = Enum(4)
   val regState = RegInit(sSetup)

   val cnt = RegInit(0.U(log2Ceil(ap.nPEs).W))
   switch (regState) {

     is (sSetup) {
       // Setup the unassigned queue
       qUnassigned.enq.valid := true.B
       qUnassigned.enq.bits := cnt
       // Set the objects to unassigned
       regAssignments(cnt) := constUnassigned.U

       when (cnt === (ap.nPEs - 1).U) {
         cnt := 0.U
         regState := sIdle
       }.otherwise {
         cnt := cnt + 1.U
       }

     }
     is (sIdle) {
       when (io.start) {
         // Dequeue an element
         when (qUnassigned.deq.valid) {
           val unassigned = WireInit(qUnassigned.deq.bits)
           regCurrentAgent := unassigned

           qUnassigned.deq.ready := true.B
           assert(qUnassigned.deq.valid)

           // Start the streamReader
           io.streamReaderCtrlSignals.start := true.B
           io.streamReaderCtrlSignals.baseAddr := getRowBaseAddr(unassigned, io.nCols, io.baseAddress)
           io.streamReaderCtrlSignals.byteCount := io.nCols * (ap.bitWidth/8).U

           regState := sReading
         }.otherwise {
           // We have no unassigned agents. => We are finished
           // At this point we must communicate the result back somehow

           io.finished := true.B
         }

       }


     }
     is (sReading) {
       // Keep start signal high so the StreamReader continues counting
       io.streamReaderCtrlSignals.start := true.B
       when (io.streamReaderCtrlSignals.finished) {
         regState := sFinished
       }
     }
     is (sFinished) {
       io.searchResultIn.ready := true.B

       when(io.searchResultIn.fire) {
         // Update unassigned and assigned and prices
         val obj = io.searchResultIn.bits.winner
         val bid = io.searchResultIn.bits.bid

         // Check if we have a valid bid (e.g. its positive)
         when (bid.asSInt > 0.S) {
           // First we must remove the agent that was previous assigned
           //  and add it to the tail of the unassigned queue
           when(objectIsAssigned(obj)) {
             qUnassigned.enq.bits := regAssignments(obj)
             qUnassigned.enq.valid := true.B
             assert(qUnassigned.enq.ready)
           }
           // Update the assignement and prices
           regAssignments(obj) := regCurrentAgent
           regPrices(obj) := bid
         }
         regState := sIdle
       }
     }

   }
 }

 */

