package auction

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.streams._

trait AuctionParams {
  def nProcessingElements : Int
  def datSz: Int
}

// Dynamic run-time parameters
class AuctionRunTimeParams extends Bundle {
  val nRows = UInt(32.W)
  val nCols = UInt(32.W)
}

// read and sum a contiguous stream of 32-bit uints from main memory
class Auction(p: PlatformWrapperParams) extends GenericAccelerator(p) {
  val numMemPorts = 1

  object ap extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 16
  }
  val io = IO(new GenericAcceleratorIF(numMemPorts, p) {
    val start = Input(Bool())
    val finished = Output(Bool())
    val baseAddr = Input(UInt(64.W))
    val nRows = Input(UInt(32.W))
    val nCols = Input(UInt(32.W))
    val byteCount = Input(UInt(32.W))
    val cycleCount = Output(UInt(32.W))
  })
  io.signature := makeDefaultSignature()
  plugMemWritePort(0)

  val rdP = new StreamReaderParams(
    streamWidth = 16, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true, useChiselQueue = true
  )


  val reader = Module(new StreamReader(rdP)).io

  val auctionController = Module(new AuctionController(ap))
  val dataDistributor = Module(new DataDistributor(ap))
  val searchTask = Module(new SearchTask(ap))
  val pe = for (i <- 0 until ap.nProcessingElements) yield {
    Module(new ProcessingElement(ap))
  }
  val peDistributor = Module(new PEsToSearchTask(ap))

  // Connect Streamreader to Datadistributor
  dataDistributor.mem <> reader.out

  // Connect DataDistributor to Processing Elements rewardIn port
  dataDistributor.peOut.zipWithIndex.map( { case (port, idx) => port <> pe(idx).io.rewardIn })

  // Connect AuctionController priceOut to PEs priceIn
  auctionController.io.pricesOut.zipWithIndex.map( {case (port, idx) => port <> pe(idx).io.priceIn})

  // Connect ProcessingElements to Search Task
  pe.zipWithIndex.map({case (pe, idx) => pe.io.benefitOut <> peDistributor.peIn(idx)})

  // Connect peDistributor to searchTask
  peDistributor.searchOut <> searchTask.io.benefitIn

  // Connect Search Results to the controller
  searchTask.io.resultOut <> auctionController.io.searchResultIn

  // Connect the auctionController to the streamReader
  val ctrl = auctionController.io.streamReaderCtrlSignals
  reader.start := ctrl.start
  reader.baseAddr := ctrl.baseAddr
  reader.byteCount := ctrl.byteCount
  ctrl.finished := reader.finished
  ctrl.active := reader.active
  ctrl.error := reader.error

 // Connect CSR to Auction controller
  auctionController.io.nCols := io.nCols
  auctionController.io.nRows := io.nRows
  auctionController.io.start := io.start
  io.finished := auctionController.io.finished
  auctionController.io.baseAddress := io.baseAddr

  //  when inspecting verilog output of chisel2 synthesis they are commented out of the
  //  module interface of StreamReader, how?
  reader.doInit := false.B
  reader.initCount := 0.U


  reader.req <> io.memPort(0).memRdReq
  io.memPort(0).memRdRsp <> reader.rsp

  val regCycleCount = RegInit(0.U(32.W))
  io.cycleCount := regCycleCount
  when(!io.start) {regCycleCount := 0.U}
    .elsewhen(io.start & !io.finished) {regCycleCount := regCycleCount + 1.U}
}


// This class connects all the PEs to the single Search Task
// TODO: Support reset?
class PEsToSearchTask(ap: AuctionParams) extends MultiIOModule {
  val peIn = IO(Vec(ap.nProcessingElements, Flipped(Decoupled(UInt(ap.datSz.W)))))
  val searchOut = IO(Decoupled(UInt(ap.datSz.W)))

  // Drive defaults
  peIn.map(_.ready := false.B)
  val cnt  = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))

  searchOut <> peIn(cnt)

  when(searchOut.fire) {
    when(cnt === (ap.nProcessingElements - 1).U) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt + 1.U
    }
  }
}


// DataDistributor connects to the memory stream and distributes the values to the PEs
class DataDistributor(ap: AuctionParams) extends MultiIOModule {
  val mem = IO(Flipped(Decoupled(UInt(ap.datSz.W))))
  val peOut = IO(Vec(ap.nProcessingElements, Decoupled(UInt(ap.datSz.W))))

  val cnt  = IO(Output(UInt(log2Ceil(ap.nProcessingElements).W)))

  val regCount = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  cnt := regCount
  // Initialize the output to 0s
  peOut.map({ (out: DecoupledIO[UInt]) =>
    out.valid := false.B
    out.bits := DontCare
  })

  // Connect the memory stream to the right PE
  mem <> peOut(regCount)

  when (mem.fire === true.B) {
    when (regCount === (ap.nProcessingElements-1).U ) {
      regCount := 0.U
    }.otherwise {
      regCount := regCount + 1.U
    }
  }
}

// ProcessingElements do the processing (subtraction) and calculates the net benefit
class ProessingElementIO(ap: AuctionParams) extends Bundle {
  val rewardIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val priceIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val benefitOut = Decoupled(UInt(ap.datSz.W))
}

class ProcessingElement(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new ProessingElementIO(ap))

  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regReward = RegInit(0.U(ap.datSz.W))
  val regPrice = RegInit(0.U(ap.datSz.W))
  val regBenefit = RegInit(0.U(ap.datSz.W))

  // Drive signals to default
  io.rewardIn.ready := false.B
  io.benefitOut.valid := false.B
  io.benefitOut.bits := DontCare
  io.priceIn.ready := false.B


  switch (regState) {
    is (sIdle) {
      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.priceIn.valid) {
        io.rewardIn.ready := true.B
        io.priceIn.ready := true.B
        regReward := io.rewardIn.bits
        regPrice := io.priceIn.bits
        regState := sProcess
      }

    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later
      regBenefit := regReward - regPrice
      regState := sFinished
    }
    is (sFinished) {
      // Expose result
      io.benefitOut.valid := true.B
      io.benefitOut.bits := regBenefit
      when (io.benefitOut.fire) {
        regState := sIdle
      }
    }
  }
}

// The search tasks takes in one net benefit at the time and calculates the
// total highest, its index and its bid which is passed along to next node
class SearchTaskResult(private val ap: AuctionParams) extends Bundle {
  val winner = UInt(log2Ceil(ap.nProcessingElements).W)
  val bid = UInt(ap.datSz.W)
}

class SearchTaskIO(ap: AuctionParams) extends Bundle {
  val benefitIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.ready := false.B
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
}

class SearchTask(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new SearchTaskIO(ap))
  val regCurrentBest = RegInit(0.U(ap.datSz.W))
  val regCurrentNextBest = RegInit(0.U(ap.datSz.W))
  val regCount = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  val regCurrentBestIdx = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))

  val sProcess :: sFinished :: Nil = Enum(2)
  val regState = RegInit(sProcess)

  // Drive interface signals to default
  io.driveDefaults

  switch (regState) {
    is (sProcess) {
      io.benefitIn.ready := true.B
      io.resultOut.valid := false.B
      io.resultOut.bits := DontCare
      when (io.benefitIn.fire) {
        when(io.benefitIn.bits > regCurrentBest && io.benefitIn.bits(ap.datSz-1) === false.B) {
          regCurrentBest := io.benefitIn.bits
          regCurrentNextBest := regCurrentBest
          regCurrentBestIdx := regCount
        }
        .otherwise
        {
          when(io.benefitIn.bits > regCurrentNextBest && io.benefitIn.bits(ap.datSz-1) === false.B) {
            regCurrentNextBest := io.benefitIn.bits
          }
        }
        // Increment count
        when(regCount === (ap.nProcessingElements - 1).U) {
          regCount := 0.U
          regState := sFinished
        }. otherwise {
          regCount := regCount + 1.U
        }
      }
    }
    is (sFinished) {
      io.benefitIn.ready := false.B
      io.resultOut.valid := true.B
      io.resultOut.bits.winner := regCurrentBestIdx
      // If we have a tie (and its valid != 0) we make a unit raise
      // Not sure if this is desired? Well well

      when (regCurrentBest === regCurrentNextBest && regCurrentBest > 0.U) {
        io.resultOut.bits.bid := 1.U
      }.otherwise{
        io.resultOut.bits.bid := regCurrentBest - regCurrentNextBest
      }

      when (io.resultOut.fire) {
        regState := sProcess
        regCurrentBest := 0.U
        regCurrentNextBest := 0.U
      }
    }
  }
}

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