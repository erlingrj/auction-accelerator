package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.ocm.FPGAQueue
import fpgatidbits.synthutils.PrintableParam

class DataMuxParams(
  val bitWidth: Int,
  val memWidth: Int,
  val nPEs: Int,
  val maxProblemSize: Int
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
}
class MemData(val ap: DataMuxParams) extends Bundle {
  val data = UInt(ap.memWidth.W)
  val mask = UInt((ap.memWidth/ap.bitWidth).W)
  val last = Bool()
}

class DataMuxIO(p: DataMuxParams) extends Bundle {
  val bramWordIn = Flipped(Decoupled(new BramMemWord(nPEs = p.nPEs, bitWidth = p.bitWidth, agentWidth = p.agentWidth)))

  val peP = new ProcessingElementParams(bitWidth = p.bitWidth,
    maxProblemSize = p.maxProblemSize, nPEs = p.nPEs
  )

  val peOut = Vec(p.nPEs, Decoupled(new PERewardIO(peP)))

}

class DataMux(p: DataMuxParams) extends Module {
  val io = IO(new DataMuxIO(p))

  val qData = Module(new FPGAQueue(new BramMemWord(p.nPEs, p.bitWidth, p.agentWidth), 2))
  io.bramWordIn <> qData.io.enq


  qData.io.deq.ready := io.peOut(0).ready
  val qDataDeq = qData.io.deq.bits

  io.peOut.zipWithIndex.map({
    case (pe, idx) =>
      pe.valid := qData.io.deq.valid
      pe.bits.reward := qDataDeq.els(idx).reward
      pe.bits.idx := qDataDeq.els(idx).idx
      pe.bits.last := qDataDeq.last
  })

}