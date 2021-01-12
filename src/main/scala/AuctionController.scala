package auction

import chisel3._
import chisel3.util._

// AuctionController does a couple of things
// 1. It keeps a list over the assignments
// 2. It keeps a list over the prices
// 3. It requests the next column from StreamReader
// 4. Input the correct prices to the PEs
// 5. Take the search result and update assignments and unassigned

class StreamReaderControlSignals extends Bundle {
  val start = Input(Bool())
  val active = Output(Bool())
  val finished = Output(Bool())
  val error = Output(Bool())
  val baseAddr = Input(UInt(64.W)) // TODO: Make generic
  val byteCount = Input(UInt(32.W))


}

class AuctionControllerIO(ap: AuctionParams) extends Bundle {
  val searchResultIn = Flipped(Decoupled(new SearchTaskResult(ap)))
  val streamReaderCtrlSignals = Flipped(new StreamReaderControlSignals)
  val pricesOut = Vec(ap.nProcessingElements, Decoupled(UInt(ap.datSz.W)))
  val start = Input(Bool())
  val finished = Output(Bool())
  val baseAddress = Input(UInt(64.W))
  val nRows = Input(UInt(32.W))
  val nCols = Input(UInt(32.W))


  def driveDefaults() = {
    finished := false.B
    pricesOut.map({case (out) =>
      out.valid := false.B
      out.bits := DontCare
    })
    searchResultIn.ready := false.B
    streamReaderCtrlSignals.start := false.B
    streamReaderCtrlSignals.byteCount := DontCare
    streamReaderCtrlSignals.baseAddr := DontCare
  }
}


class AuctionController(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new AuctionControllerIO(ap))
  io.driveDefaults

  // getRowBaseAddr returns the start-address for a given row.
  def getRowBaseAddr(rowIdx: UInt, nCols: UInt, baseAddr: UInt): UInt = {
    baseAddr + rowIdx*nCols*(ap.datSz/8).U
  }

  val constUnassigned = ap.nProcessingElements
  val constAssSz = log2Ceil(ap.nProcessingElements)
  // This function looks up object at index <obj> and checks if the MSb is set
  def objectIsAssigned(obj: UInt): Bool = {
    (regAssignments(obj) >> constAssSz).asUInt === 0.U
  }

  // Initialize the agent->object assignments. Unassigned == high bit set
  val regAssignments = RegInit(VecInit(Seq.fill(ap.nProcessingElements){
    0.U((log2Ceil(ap.nProcessingElements) + 1).W)}))
  val qUnassigned = Module(new Queue(UInt(), ap.nProcessingElements)).io
  qUnassigned.deq.ready := false.B
  qUnassigned.enq.valid := false.B
  qUnassigned.enq.bits := DontCare

  val regCurrentAgent= RegInit(0.U)

  val regPrices = RegInit(VecInit(Seq.fill(ap.nProcessingElements){0.U(ap.datSz.W)}))
  // Connect prices to the PEs
  (io.pricesOut zip regPrices).map({
    case (io, reg) =>
      io.valid := true.B
      io.bits := reg
  })



  val sSetup :: sIdle :: sReading :: sFinished :: Nil = Enum(4)
  val regState = RegInit(sSetup)

  val cnt = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  switch (regState) {

    is (sSetup) {
      // Setup the unassigned queue
      qUnassigned.enq.valid := true.B
      qUnassigned.enq.bits := cnt
      // Set the objects to unassigned
      regAssignments(cnt) := constUnassigned.U

      when (cnt === (ap.nProcessingElements - 1).U) {
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
          io.streamReaderCtrlSignals.byteCount := io.nCols * (ap.datSz/8).U

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
