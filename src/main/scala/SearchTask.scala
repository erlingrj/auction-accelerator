package auction

import chisel3._
import chisel3.util._


// The search tasks takes in one net benefit at the time and calculates the
// total highest, its index and its bid which is passed along to next node
class SearchTaskResult(private val ap: AuctionParams) extends Bundle {
  val winner = UInt(log2Ceil(ap.nProcessingElements).W)
  val bid = UInt(ap.datSz.W)
}

class SearchTaskIO(ap: AuctionParams) extends Bundle {
  val benefitIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.ready := false.B
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
}

class SearchTask(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new SearchTaskIO(ap))
  val regCurrentBest = RegInit(0.U(ap.datSz.W))
  val regCurrentNextBest = RegInit(0.U(ap.datSz.W))
  val regCount = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))
  val regCurrentBestIdx = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))

  val sProcess :: sFinished :: Nil = Enum(2)
  val regState = RegInit(sProcess)

  // Drive interface signals to default
  io.driveDefaults

  switch (regState) {
    is (sProcess) {
      io.benefitIn.ready := true.B
      io.resultOut.valid := false.B
      io.resultOut.bits := DontCare
      when (io.benefitIn.fire) {
        when(io.benefitIn.bits > regCurrentBest && io.benefitIn.bits(ap.datSz-1) === false.B) {
          regCurrentBest := io.benefitIn.bits
          regCurrentNextBest := regCurrentBest
          regCurrentBestIdx := regCount
        }
          .otherwise
          {
            when(io.benefitIn.bits > regCurrentNextBest && io.benefitIn.bits(ap.datSz-1) === false.B) {
              regCurrentNextBest := io.benefitIn.bits
            }
          }
        // Increment count
        when(regCount === (ap.nProcessingElements - 1).U) {
          regCount := 0.U
          regState := sFinished
        }. otherwise {
          regCount := regCount + 1.U
        }
      }
    }
    is (sFinished) {
      io.benefitIn.ready := false.B
      io.resultOut.valid := true.B
      io.resultOut.bits.winner := regCurrentBestIdx
      // If we have a tie (and its valid != 0) we make a unit raise
      // Not sure if this is desired? Well well

      when (regCurrentBest === regCurrentNextBest && regCurrentBest > 0.U) {
        io.resultOut.bits.bid := 1.U
      }.otherwise{
        io.resultOut.bits.bid := regCurrentBest - regCurrentNextBest
      }

      when (io.resultOut.fire) {
        regState := sProcess
        regCurrentBest := 0.U
        regCurrentNextBest := 0.U
      }
    }
  }
}

