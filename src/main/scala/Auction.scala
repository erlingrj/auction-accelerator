package auction

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.ocm.{FPGAQueue, SimpleDualPortBRAM, SinglePortBRAM}
import fpgatidbits.streams._
import fpgatidbits.synthutils.PrintableParam


class AuctionParams(
  val nPEs: Int,
  val bitWidth: Int,
  val maxProblemSize: Int,
  val memWidth: Int,
) extends PrintableParam
{
  def headersAsList(): List[String] = {
    List(
      "nPEs", "bitWidth", "maxProblemSize", "memWidth"
    )
  }

  def contentAsList(): List[String] = {
    List(nPEs, bitWidth, maxProblemSize, memWidth).map(_.toString)
  }

  def agentWidth: Int = log2Ceil(maxProblemSize)
  def pricesPerPE: Int = maxProblemSize/nPEs
  def pricesPerPEWidth: Int = log2Ceil(pricesPerPE)
  def bramDataWidth: Int = (bitWidth + 1 + agentWidth)*nPEs // Enough to store data + col + last for each PE
  def bramAddrWidth: Int = log2Ceil(maxProblemSize*maxProblemSize / (nPEs)) //Enough to store max problemsize

  def agentRowStoreParams: RegStoreParams = new RegStoreParams(1,1,0, agentWidth)
  def priceRegStoreParams: RegStoreParams = new RegStoreParams(nPEs + 1,0 ,1,agentWidth)
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
    maxBeats = 1, chanID = 0, disableThrottle = true, useChiselQueue = false
  )
  // Create all the submodules
  val mcP = new MemCtrlParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val memController = Module(new BramController(mcP))

  val cP = new ApplicationControllerParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val controller = Module(new ApplicationController(cP))

  val aP = new AssignmentEngineParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val accountant = Module(new AssignmentEngine(aP))

  val ddP = new DataMuxParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, memWidth = mp.dataWidth
  )
  val dataMux = Module(new DataMux(ddP))

  // create some queues


  // On-chip-memory
  val bram = Module(new SimpleDualPortBRAM(addrBits=ap.bramAddrWidth, dataBits=ap.bramDataWidth))
  val agentRowStore = Module(new RegStore(gen=new AgentRowInfo(p=mcP), p=ap.agentRowStoreParams))

  val bsP = new BramStoreParams(bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs)
  val priceStore = Module(new BramStore(bsP))


  priceStore.reset := controller.io.reinit
  agentRowStore.reset := controller.io.reinit


  // dram2bram reader
  val dram2bram = Module(new Dram2Bram2(p = mcP))
  dram2bram.io.dramReq <> io.memPort(0).memRdReq
  dram2bram.io.dramRsp <> io.memPort(0).memRdRsp
  dram2bram.io.bramCmd <> bram.io.write
  dram2bram.io.start := controller.io.dram2bram_start
  dram2bram.io.baseAddr := controller.io.dram2bram_baseAddr
  dram2bram.io.nRows := controller.io.dram2bram_nRows
  dram2bram.io.nCols := controller.io.dram2bram_nCols
  dram2bram.io.agentRowAddress <> agentRowStore.io.wPorts(0)
  controller.io.dram2bram_finished := dram2bram.io.finished

  dram2bram.io.nElements := controller.io.nElements


  // Dram writer of the result
  val wrP = new StreamWriterParams(
    streamWidth = p.memDataBits,
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
  memWriter.io.baseAddr := accountant.io.writeBackStream.baseAddr
  memWriter.io.byteCount := accountant.io.writeBackStream.byteCount //TODO : this should be a parameter. Its nObjects * bytes * 2 (we use 32bits = 4)
  accountant.io.writeBackStream.finished := memWriter.io.finished

  val peP = new ProcessingElementParams(
    bitWidth = ap.bitWidth, nPEs = ap.nPEs, maxProblemSize = ap.maxProblemSize
  )

  val pes = for (i <- 0 until ap.nPEs) yield {
    Module(new ProcessingElement(peP))
  }

  val stP = new SearchTaskParams(
    bitWidth = ap.bitWidth, nPEs = ap.nPEs, maxProblemSize = ap.maxProblemSize
  )
  val search = Module(new SearchTask(stP))


  memController.io.dataDistOut <> dataMux.io.bramWordIn
  memController.io.agentRowAddrReq <> agentRowStore.io.rPorts(0)
  memController.io.bramReq <> bram.io.read

  for (i <- 0 until ap.nPEs) {
    dataMux.io.peOut(i) <> pes(i).io.rewardIn
    pes(i).io.PEResultOut <> search.io.benefitIn(i)
    pes(i).io.price <> priceStore.io.peReadRsp(i)
    priceStore.io.peReadReq(i) := pes(i).io.agentIdx
  }



  search.io.resultOut <> accountant.io.searchResultIn
  accountant.io.bramStoreReadData := priceStore.io.assReadRsp
  priceStore.io.assReadReq := accountant.io.bramStoreReadAddr

  priceStore.io.assWriteReq.valid := accountant.io.bramStoreWriteDataValid
  priceStore.io.assWriteReq.bits.addr := accountant.io.bramStoreWriteAddr
  priceStore.io.assWriteReq.bits.data := accountant.io.bramStoreWriteData


  priceStore.io.dumpOut <> accountant.io.bramStoreDump
  priceStore.io.dump := accountant.io.bramStoreDumpStart


  // MemController <> qUnassigned <> Controller <> Auction
  memController.io.unassignedAgents <> controller.io.unassignedAgentsOut
  controller.io.unassignedAgentsIn <> accountant.io.unassignedAgents

  memController.io.requestedAgents <> controller.io.requestedAgentsIn
  controller.io.requestedAgentsOut <> accountant.io.requestedAgents

  controller.io.writeBackDone := accountant.io.writeBackDone
  accountant.io.doWriteBack := controller.io.doWriteBack
  accountant.io.rfInfo := io.rfIn

  controller.io.rfInfo := io.rfIn
  io.rfOut := controller.io.rfCtrl


  val dataMovementDone = RegInit(false.B)
  val calcDone = RegInit(false.B)
  val running = RegInit(false.B)
  val regCycleCount = RegInit(0.U(32.W))

  calcDone := controller.io.doWriteBack
  io.rfOut.cycleCount := regCycleCount
  when(running && dataMovementDone && !calcDone) {
    regCycleCount := regCycleCount + 1.U
  }.elsewhen(!running && io.rfIn.start) {
    regCycleCount := 0.U
  }

  when(!running) {
    running := io.rfIn.start
  } otherwise {
    running := !controller.io.rfCtrl.finished
  }

  when (!dataMovementDone) {
    dataMovementDone := dram2bram.io.finished
  } otherwise {
    dataMovementDone := !controller.io.rfCtrl.finished
  }
}





