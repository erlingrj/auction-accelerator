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

class DataDistributorIO(private val ap: DataDistributorParams) extends Bundle {
  val mem = Flipped(Decoupled(new MemData(ap)))
  val peP = new ProcessingElementParams(bitWidth = ap.bitWidth,
    maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs
  )

  val peOut = Vec(ap.nPEs, Decoupled(new PERewardIO(peP)))
}

// DD Par UnOrdered. This one also accepts a mask from the mem-response
//  this way it can support problem sizes which exceed the nPEs
class DataDistributorParUnO(ap: DataDistributorParams) extends MultiIOModule {
  val io = IO(new DataDistributorIO(ap))

  require(ap.memWidth % ap.bitWidth == 0, "[DD] Aligned mem access only")

  def getSubWord(word: UInt, idx: Int): UInt = {
    word((idx+1)*ap.bitWidth-1, idx*ap.bitWidth)
  }

  val qData = Module(new Queue(new MemData(ap), 8))
  io.mem <> qData.io.enq


  qData.io.deq.ready := io.peOut(0).ready


  io.peOut.zipWithIndex.map({
    case (pe, idx) =>
      pe.valid := qData.io.deq.valid && qData.io.deq.bits.mask(idx)
      pe.bits.reward := getSubWord(qData.io.deq.bits.data, idx)
      pe.bits.last := qData.io.deq.bits.last
  })
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