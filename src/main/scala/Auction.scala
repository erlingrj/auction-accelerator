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





