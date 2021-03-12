package auction

import chisel3._
import chisel3.util._
import fpgatidbits.PlatformWrapper._
import fpgatidbits.dma._
import fpgatidbits.streams._
import fpgatidbits.synthutils.PrintableParam



class AuctionParams(
                   val nPEs: Int,
                   val bitWidth: Int,
                   val maxProblemSize: Int,
                   val memWidth: Int
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
}
/*
trait AuctionParams {
  def nPEs : Int
  def bitWidth: Int
  def maxProblemSize: Int
  def memWidth: Int

  def agentWidth: Int = log2Ceil(maxProblemSize)
  def pricesPerPE: Int = maxProblemSize/nPEs
  def pricesPerPEWidth: Int = log2Ceil(pricesPerPE)
}
*/


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
  val mcP = new MemCtrlParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val memController = Module(new AuctionDRAMController(mcP))

  val cP = new ControllerParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val controller = Module(new Controller(cP))

  val aP = new AccountantParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, mrp = mp
  )
  val accountant = Module(new AccountantNonPipelined(aP))

  val ddP = new DataDistributorParams( bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize,
    nPEs = ap.nPEs, memWidth = mp.dataWidth
  )
  val dataMux = Module(new DataDistributorParUnO(ddP))

  // create some queues
  val qUnassignedAgents = Module(new Queue(gen=new AgentInfo(ap.bitWidth), entries=16))
  val qRequestedAgents = Module(new Queue(gen=new AgentInfo(ap.bitWidth), entries=16))




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
  memWriter.io.baseAddr := io.rfIn.baseAddrRes
  memWriter.io.byteCount := io.rfIn.nObjects*(8*2).U //TODO : this should be a parameter. Its nObjects * bytes * 2 (we use 32bits = 4)
  accountant.io.writeBackStream.finished := memWriter.io.finished

  val peP = new ProcessingElementParams(
    bitWidth = ap.bitWidth, nPEs = ap.nPEs, maxProblemSize = ap.maxProblemSize
  )

  val pes = for (i <- 0 until ap.nPEs) yield {
    Module(new ProcessingElementPar(peP,i))
  }

  val stP = new SearchTaskParams(
    bitWidth = ap.bitWidth, nPEs = ap.nPEs, maxProblemSize = ap.maxProblemSize
  )
  val search = Module(new SearchTaskPar(stP))

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

  // MemController <> qUnassigned <> Controller <> Auction
  memController.ioCtrl.unassignedAgents <> qUnassignedAgents.io.deq
  qUnassignedAgents.io.enq <> controller.io.unassignedAgentsOut
  controller.io.unassignedAgentsIn <> accountant.io.unassignedAgents

  memController.ioCtrl.requestedAgents <> qRequestedAgents.io.enq
  qRequestedAgents.io.deq <> controller.io.requestedAgentsIn
  controller.io.requestedAgentsOut <> accountant.io.requestedAgents



  controller.io.writeBackDone := accountant.io.writeBackDone
  accountant.io.doWriteBack := controller.io.doWriteBack
  accountant.io.rfInfo := io.rfIn

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





