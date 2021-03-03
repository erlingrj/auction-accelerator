package auction

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.streams._

trait AuctionParams {
  def nPEs : Int
  def bitWidth: Int
  def maxProblemSize: Int
  def memWidth: Int

  def agentWidth: Int = log2Ceil(maxProblemSize)
  def pricesPerPE: Int = maxProblemSize/nPEs
  def pricesPerPEWidth: Int = log2Ceil(pricesPerPE)
}

class AppControlSignals extends Bundle {
  val finished = Output(Bool())
  val cycleCount = Output(UInt(32.W))
}

class AppInfoSignals extends Bundle {
  val start = Input(Bool())
  val baseAddr = Input(UInt(64.W))
  val nAgents = Input(UInt(32.W))
  val nObjects = Input(UInt(32.W))
  val baseAddrRes = Input(UInt(64.W))
}

// read and sum a contiguous stream of 32-bit uints from main memory
class Auction(p: PlatformWrapperParams, ap: AuctionParams) extends GenericAccelerator(p) {
  val numMemPorts = 1

  val io = IO(new GenericAcceleratorIF(numMemPorts, p) {
    val rfOut = new AppControlSignals()
    val rfIn = new AppInfoSignals()
  })
  io.signature := makeDefaultSignature()
  plugMemWritePort(0)

  val mp = p.toMemReqParams()

  val rdP = new StreamReaderParams(
    streamWidth = ap.memWidth, fifoElems = 8, mem = p.toMemReqParams(),
    maxBeats = 1, chanID = 0, disableThrottle = true, useChiselQueue = true
  )
  // Create all the submodules
  val memController = Module(new AuctionDRAMController(ap,mp))
  val controller = Module(new Controller(ap,mp))
  val accountant = Module(new AccountantNonPipelined(ap,mp))
  val dataMux = Module(new DataDistributorParUnO(ap))

  val wrP = new StreamWriterParams(
    streamWidth = ap.bitWidth,
    mem = p.toMemReqParams(),
    chanID = 0,
    maxBeats = 1
  )

  val memWriter = Module(new StreamWriter(wrP))
  memWriter.io.req <> io.memPort(0).memWrReq
  memWriter.io.rsp <> io.memPort(0).memWrRsp
  io.memPort(0).memWrDat <> memWriter.io.wdat

  memWriter.io.in <> accountant.io.writeBackStream.wrData
  memWriter.io.start := accountant.io.writeBackStream.start
  memWriter.io.baseAddr := io.rfIn.baseAddrRes
  memWriter.io.byteCount := io.rfIn.nObjects*8.U //TODO : this should be a parameter. Its nObjects * bytes * 2 (we use 32bits = 4)


  val pes = for (i <- 0 until ap.nPEs) yield {
    Module(new ProcessingElementPar(ap,i))
  }
  val search = Module(new SearchTaskPar(ap))

  memController.ioMem.req <> io.memPort(0).memRdReq
  memController.ioMem.rsp <> io.memPort(0).memRdRsp

  memController.ioCtrl.regFile <> io.rfIn
  memController.ioCtrl.memData <> dataMux.io.mem

  for (i <- 0 until ap.nPEs) {
    dataMux.io.peOut(i) <> pes(i).io.rewardIn
    pes(i).io.PEResultOut <> search.io.benefitIn(i)
    pes(i).io.controlIn <> accountant.io.PEControlOut(i)
  }

  search.io.resultOut <> accountant.io.searchResultIn
  controller.io.memoryRequestIn <> accountant.io.memoryRequest
  controller.io.memoryRequestedOut <> accountant.io.memoryRequested
  controller.io.writeBackDone := accountant.io.writeBackDone
  accountant.io.doWriteBack := controller.io.doWriteBack
  accountant.io.rfInfo := io.rfIn

  controller.io.memoryRequestedIn <> memController.ioCtrl.requestedAgents
  controller.io.memoryRequestOut <> memController.ioCtrl.unassignedAgents
  controller.io.rfInfo := io.rfIn
  io.rfOut := controller.io.rfCtrl


  val regCycleCount = RegInit(0.U(32.W))
  io.rfOut.cycleCount := regCycleCount
  when(!io.rfIn.start) {
    regCycleCount := 0.U
  }.elsewhen(io.rfIn.start & !io.rfOut.finished) {
    regCycleCount := regCycleCount + 1.U
  }
}





