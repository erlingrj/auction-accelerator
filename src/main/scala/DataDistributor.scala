package auction

import chisel3._
import chisel3.util._

class MemData(private val ap: AuctionParams) extends Bundle {
  val data = UInt(ap.memWidth.W)
  val mask = UInt((ap.memWidth/ap.bitWidth).W)
  val last = Bool()
}

class DataDistributorIO(private val ap: AuctionParams) extends Bundle {
  val mem = Flipped(Decoupled(new MemData(ap)))
  val peOut = Vec(ap.nPEs, Decoupled(new PERewardIO(ap)))
}

// DD Par UnOrdered. This one also accepts a mask from the mem-response
//  this way it can support problem sizes which exceed the nPEs
class DataDistributorParUnO(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new DataDistributorIO(ap))

  require(ap.memWidth % ap.bitWidth == 0, "[DD] Aligned mem access only")

  def getSubWord(word: UInt, idx: Int): UInt = {
    word(idx*ap.bitWidth, (idx+1)*ap.bitWidth)
  }

  io.mem.ready := io.peOut(0).ready

  io.peOut.zipWithIndex.map({
    case (pe, idx) =>
      pe.valid := io.mem.valid && io.mem.bits.mask(idx)
      pe.bits.reward := getSubWord(io.mem.bits.data, idx)
      pe.bits.last := io.mem.bits.last
  })


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