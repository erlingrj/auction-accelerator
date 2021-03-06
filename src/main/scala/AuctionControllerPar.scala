// package auction

 import chisel3._
 import chisel3.util._
 import fpgatidbits.dma.MemReqParams

 // AuctionController does a couple of things
 // 1. It keeps a list over the assignments
 // 2. It keeps a list over the prices
 // 3. It requests the next column from StreamReader
 // 4. Input the correct prices to the PEs
 // 5. Take the search result and update assignments and unassigned
/*

 abstract class AuctionControllerPar(ap: AuctionParams, mp: MemReqParams) extends MultiIOModule {
   val io = IO(new AuctionControllerParIO(ap,mp))

   def getStartAddrForRow(rowIdx: UInt, nCols: UInt, baseAddr: UInt): UInt = {
     baseAddr + rowIdx*nCols*(ap.bitWidth/8).U
   }

 }


 class AuctionControllerParIO(ap: AuctionParams,mp: MemReqParams) extends Bundle {
   val searchResultIn = Flipped(Decoupled(new SearchTaskResult(ap)))

   // MemoryRqeust and memoryRequested are the two interfaces to the queues to Memory Controller
   val memoryRequest = Decoupled(new AgentInfo(ap,mp))
   val memoryRequested = Decoupled(new AgentInfo(ap,mp))

   val PEControlOut = Vec(ap.nPEs, Decoupled(new PEControl(ap)))

   val appCtrl = new AppControlSignals()

   def driveDefaults(): Unit = {
     appCtrl.finished := false.B
     PEControlOut.map({case (out) =>
       out.valid := false.B
       out.bits := DontCare
     })
     searchResultIn.ready := false.B
     memoryRequest.valid := false.B
     memoryRequest.bits := DontCare
     memoryRequested.ready := false.B
   }
 }

 class Assignment(private val ap: AuctionParams) extends Bundle {
   val agent = UInt(ap.agentWidth.W)
   val valid = Bool()
 }

 // First try on the parallel auction controller. A non-pipelined version
 class AuctionControllerNonPipelined(ap: AuctionParams, mp: MemReqParams)
   extends AuctionControllerPar(ap,mp)
 {
   val regAssignments = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U.asTypeOf(new Assignment(ap)))))
   val regCurrentAgent = RegInit(0.U)
   val regPrices = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U(ap.bitWidth.W))))

   // Connect prices to output
   var regPriceIdx = 0
   for (i <- 0 until ap.nPEs) {
     io.PEControlOut(i).valid := true.B
     for (j <- 0 until ap.maxProblemSize/ap.nPEs) {
       io.PEControlOut(i).bits.prices(j) := regPrices(regPriceIdx)
       regPriceIdx += 1
     }
   }

   val regObject = RegInit(0.U(ap.agentWidth.W))
   val regBid = RegInit(0.U(ap.bitWidth.W))

   // State machine
   val sIdle :: sSetup :: sProcess :: sReading :: sPostProcess :: sWriteBack :: Nil = Enum(6)
   val regState = RegInit(sIdle)
   val regInitCount = 0.U(ap.agentWidth.W)

   switch (regState) {
     is (sIdle) {
       when (io.appCtrl.start === true.B) {
         regState := sSetup
       }
     }
     is (sSetup) {
       when (regInitCount === io.appCtrl.nObjects) {
         regState := sProcess
       }.otherwise{
         qUnassigned.enq.valid := true.B
         qUnassigned.enq.bits := regInitCount
       }
       regInitCount := regInitCount + 1.U
     }

     is (sProcess) {

       // Dequeue an element
       when (qUnassigned.deq.valid) {
         val unassigned = WireInit(qUnassigned.deq.bits)
         qUnassigned.deq.ready := true.B
         assert(qUnassigned.deq.valid)
         qUnassignedRequested.enq.valid := true.B
         qUnassignedRequested.enq.bits := unassigned


         // Read out the prices for that guy
         io.memoryRequest.valid := true.B
         io.memoryRequest.bits.agent := unassigned
         io.memoryRequest.bits.nObjects := io.appCtrl.nObjects
         assert(io.memoryRequest.ready === true.B)

         regState := sReading
       }.otherwise {
         // We have no unassigned agents. => We are finished
         // At this point we must communicate the result back somehow
         regState := sWriteBack
       }
     }
     is (sReading) {
       io.searchResultIn.ready := true.B
       when (io.searchResultIn.fire()) {
         io.memoryRequested.ready := true.B
         assert(io.memoryRequested.valid === true.B)
         regObject := io.searchResultIn.bits.winner
         regBid := io.searchResultIn.bits.bid
         regCurrentAgent := io.memoryRequested.bits.agent
         regState := sPostProcess
       }
     }

     is (sPostProcess) {
       when (regBid > 0.U) {
         // We have a valid bid but we have to check whether the bid already theres is higher
         when(regAssignments(regObject).valid && (regPrices(regObject) > regBid))  {
             io.memoryRequest.valid := true.B
             io.memoryRequest.bits.agent := regCurrentAgent
             io.memoryRequest.bits.nObjects := io.appCtrl.nObjects
             assert(io.memoryRequest.ready === true.B)
         }.otherwise {
           qUnassigned.enq.valid := regAssignments(regObject).valid
           qUnassigned.enq.bits := regAssignments(regObject).agent
           assert(qUnassigned.enq.ready === true.B)

           regAssignments(regObject) := regCurrentAgent
           regPrices(regObject) := regBid
         }
       }
       regState := sProcess
     }
     is (sWriteBack)  {
       // TODO: How is the result written back
       regState := sIdle
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
 */