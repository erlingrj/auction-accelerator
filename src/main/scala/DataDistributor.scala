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

class DataDistributorReducer(ap: DataDistributorParams) extends Bundle {

}
/*

// ParInO deals with multiple PEs but its all "in-order" or
//  it only deals with the case where memWidth = nPEs * bitWidth
class DataDistributorParInO(ap: AuctionParams) extends MultiIOModule {

  val io = IO(new DataDistributorIO((ap)))

  // Some assertions
  assert(ap.memWidth == ap.bitWidth*ap.nPEs, "Each mem op must feed exactly one batch of PEs")
  io.peOut.map(p => assert(p.ready === io.peOut(0).ready, "All peOut must have identical ready signalling"))

  io.mem.ready := io.peOut(0).ready

  // Connect the peOuts directly to memory, no buffering
  io.peOut.zipWithIndex.map({ (t) =>
    val pe = t._1
    val idx = t._2
    pe.valid := io.mem.valid
    pe.bits := io.mem.bits.data(idx*ap.bitWidth, (idx+1)*ap.bitWidth)
  })

}
class DataDistributor(ap: AuctionParams) extends MultiIOModule {
  val mem = IO(Flipped(Decoupled(UInt(ap.bitWidth.W))))
  val peOut = IO(Vec(ap.nPEs, Decoupled(UInt(ap.bitWidth.W))))

  val cnt  = IO(Output(UInt(log2Ceil(ap.nPEs).W)))

  val regCount = RegInit(0.U(log2Ceil(ap.nPEs).W))
  cnt := regCount
  // Initialize the output to 0s
  peOut.map({ (out: DecoupledIO[UInt]) =>
    out.valid := false.B
    out.bits := DontCare
  })

  // Connect the memory stream to the right PE
  mem <> peOut(regCount)

  when (mem.fire === true.B) {
    when (regCount === (ap.nPEs-1).U ) {
      regCount := 0.U
    }.otherwise {
      regCount := regCount + 1.U
    }
  }
}

 */