package auction

import chisel3._
import chisel3.util._

class DataDistributorPar(ap: AuctionParams) extends MultiIOModule {
  val mem = IO(Flipped(Decoupled(UInt(ap.memWidth.W))))
  val peOut = IO(Vec(ap.nPEs, Decoupled(UInt(ap.bitWidth.W))))

  // Some assertions
  assert(ap.memWidth == ap.bitWidth*ap.nPEs, "Each mem op must feed exactly one batch of PEs")
  peOut.map(p => assert(p.ready === peOut(0).ready, "All peOut must have identical ready signalling"))

  mem.ready := peOut(0).ready

  // Connect the peOuts directly to memory, no buffering
  peOut.zipWithIndex.map({ (t) =>
    val pe = t._1
    val idx = t._2
    pe.valid := mem.valid
    pe.bits := mem.bits(idx*ap.bitWidth, (idx+1)*ap.bitWidth)
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