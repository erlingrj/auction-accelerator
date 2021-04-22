package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.synthutils.PrintableParam

// THe accountant keeps the books on assignments and prices and gives the prices to the PEs


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

abstract class Accountant(ap: AccountantParams) extends MultiIOModule {
  val io = IO(new AccountantIO(ap))

  def getStartAddrForRow(rowIdx: UInt, nCols: UInt, baseAddr: UInt): UInt = {
    baseAddr + rowIdx*nCols*(ap.bitWidth/8).U
  }
  io.driveDefaults()
}


class WriteBackStream(ap: AccountantParams) extends Bundle {
  val start = Output(Bool())
  val wrData = Decoupled(UInt(64.W)) // TODO: One-size for prices AND agents? Or one streamwriter per?
  val finished = Input(Bool())
}

class AccountantIO(ap: AccountantParams) extends Bundle {
  val searchTaskParams = new SearchTaskParams(
    bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs
  )

  val searchResultIn = Flipped(Decoupled(new SearchTaskResult(searchTaskParams)))

  // MemoryRqeust and memoryRequested are the two interfaces to the queues to Memory Controller
  val unassignedAgents = Decoupled(new AgentInfo(ap.bitWidth))
  val requestedAgents = Flipped(Decoupled(new AgentInfo(ap.bitWidth)))

  val peParams = new ProcessingElementParams(
    bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs
  )
  val PEControlOut = Vec(ap.nPEs, Decoupled(new PEControl(peParams)))

  val rfInfo = new AppInfoSignals()

  val doWriteBack = Input(Bool())
  val writeBackDone = Output(Bool())

  val writeBackStream = new WriteBackStream(ap)

  def driveDefaults(): Unit = {
    PEControlOut.map({case (out) =>
      out.valid := false.B
      out.bits := DontCare
    })
    searchResultIn.ready := false.B
    unassignedAgents.valid := false.B
    unassignedAgents.bits := DontCare
    requestedAgents.ready := false.B
    writeBackDone := false.B
    writeBackStream.start := false.B
    writeBackStream.wrData.valid := false.B
    writeBackStream.wrData.bits := DontCare
  }
}

class Assignment(private val ap: AccountantParams) extends Bundle {
  val agent = UInt(ap.agentWidth.W)
  val valid = Bool()
}

// First try on the parallel auction controller. A non-pipelined version
class AccountantNonPipelined(ap: AccountantParams)
  extends Accountant(ap)
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
        regPrices(regObject) := regPrices(regObject) + regBid

      // Check if we were able to fire off new memory request (or maybe we didnt have to)
      when(io.unassignedAgents.fire || !newRequest) {
          regState := sWaitForBid
        }.otherwise { // If we werent able to fire the memory request. Redo the update stage
          regState := sUpdate
        }
        /*
        when(regAssignments(regObject).valid && (regPrices(regObject) > regBid))  {
          io.unassignedAgents.valid := true.B
          newRequest := true.B
          io.unassignedAgents.bits.agent := regCurrentAgent
          io.unassignedAgents.bits.nObjects := io.rfInfo.nObjects
          assert(io.unassignedAgents.ready === true.B)
        }.otherwise {
          // Kick out the potentially old guy
          io.unassignedAgents.valid := regAssignments(regObject).valid
          newRequest := regAssignments(regObject).valid
          io.unassignedAgents.bits.agent := regAssignments(regObject).agent
          io.unassignedAgents.bits.nObjects := io.rfInfo.nObjects
          assert(io.unassignedAgents.ready === true.B)

          // Update assignments and prices
          regAssignments(regObject).agent:= regCurrentAgent
          regAssignments(regObject).valid := true.B

          regPrices(regObject) := regBid
        }
         */
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
        io.writeBackStream.wrData.bits := regPrices(regWBCount)
        when (io.writeBackStream.wrData.fire()) {
          regWBCount := regWBCount + 1.U
        }
      }
    }
    is (sWaitForFinished) {
      io.writeBackStream.start := true.B
      when (io.writeBackStream.finished === true.B) {
        regState := sWaitForBid
        io.writeBackDone := true.B
      }
    }
  }

  // We run a separate state machine for the outputting of prices to the PEs
  val sIdlePrices :: sWaitForUpdate :: Nil = Enum(2)
  val regStatePrices = RegInit(sIdlePrices)

  switch (regStatePrices) {
    is (sIdlePrices) {
      io.PEControlOut.map(_.valid := true.B)

      // PE access prices
      when (io.PEControlOut.map(_.fire()).reduce(_||_)) {
        regStatePrices := sWaitForUpdate
      }
    }

    // We dont allow the PEs to dequeue any prices until we have the update
    is (sWaitForUpdate) {
      io.PEControlOut.map(_.valid := false.B)

      // When we reach the sUpdate stage we know that next iteration we have update the prices and can have valid price output
      when(regState === sUpdate) {
        regStatePrices := sIdlePrices
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

