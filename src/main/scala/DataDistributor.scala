package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.synthutils.PrintableParam

class DataDistributorParams(
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
class MemData(val ap: DataDistributorParams) extends Bundle {
  val data = UInt(ap.memWidth.W)
  val mask = UInt((ap.memWidth/ap.bitWidth).W)
  val last = Bool()
}

class DataDistributorSparseIO(p: DataDistributorParams) extends Bundle {
  val bramWordIn = Flipped(Decoupled(new BramMemWord(nPEs = p.nPEs, bitWidth = p.bitWidth, agentWidth = p.agentWidth)))

  val peP = new ProcessingElementParams(bitWidth = p.bitWidth,
    maxProblemSize = p.maxProblemSize, nPEs = p.nPEs
  )

  val peOut = Vec(p.nPEs, Decoupled(new PEExtPriceRewardIO(peP)))

}

class DataDistributorSparse(p: DataDistributorParams) extends Module {
  val io = IO(new DataDistributorSparseIO(p))

  val qData = Module(new Queue(new BramMemWord(p.nPEs, p.bitWidth, p.agentWidth), 8))
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