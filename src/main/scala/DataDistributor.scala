package auction

import chisel3._
import chisel3.util._

// DataDistributor connects to the memory stream and distributes the values to the PEs
class DataDistributor(ap: AuctionParams) extends MultiIOModule {
  val mem = IO(Flipped(Decoupled(UInt(ap.datSz.W))))
  val peOut = IO(Vec(ap.nProcessingElements, Decoupled(UInt(ap.datSz.W))))

  val cnt  = IO(Output(UInt(log2Ceil(ap.nProcessingElements).W)))

  val regCount = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  cnt := regCount
  // Initialize the output to 0s
  peOut.map({ (out: DecoupledIO[UInt]) =>
    out.valid := false.B
    out.bits := DontCare
  })

  // Connect the memory stream to the right PE
  mem <> peOut(regCount)

  when (mem.fire === true.B) {
    when (regCount === (ap.nProcessingElements-1).U ) {
      regCount := 0.U
    }.otherwise {
      regCount := regCount + 1.U
    }
  }
}