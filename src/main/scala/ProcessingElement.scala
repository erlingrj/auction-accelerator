package auction

import chisel3._
import chisel3.util._


// This class connects all the PEs to the single Search Task
// TODO: Support reset?
class PEsToSearchTask(ap: AuctionParams) extends MultiIOModule {
  val peIn = IO(Vec(ap.nProcessingElements, Flipped(Decoupled(UInt(ap.datSz.W)))))
  val searchOut = IO(Decoupled(UInt(ap.datSz.W)))

  // Drive defaults
  peIn.map(_.ready := false.B)
  val cnt  = RegInit(0.U(log2Ceil(ap.nProcessingElements).W))

  searchOut <> peIn(cnt)

  when(searchOut.fire) {
    when(cnt === (ap.nProcessingElements - 1).U) {
      cnt := 0.U
    }.otherwise {
      cnt := cnt + 1.U
    }
  }
}

// ProcessingElements do the processing (subtraction) and calculates the net benefit
class ProessingElementIO(ap: AuctionParams) extends Bundle {
  val rewardIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val priceIn = Flipped(Decoupled(UInt(ap.datSz.W)))
  val benefitOut = Decoupled(UInt(ap.datSz.W))
}

class ProcessingElement(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new ProessingElementIO(ap))

  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regReward = RegInit(0.U(ap.datSz.W))
  val regPrice = RegInit(0.U(ap.datSz.W))
  val regBenefit = RegInit(0.U(ap.datSz.W))

  // Drive signals to default
  io.rewardIn.ready := false.B
  io.benefitOut.valid := false.B
  io.benefitOut.bits := DontCare
  io.priceIn.ready := false.B


  switch (regState) {
    is (sIdle) {
      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.priceIn.valid) {
        io.rewardIn.ready := true.B
        io.priceIn.ready := true.B
        regReward := io.rewardIn.bits
        regPrice := io.priceIn.bits
        regState := sProcess
      }

    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later
      regBenefit := regReward - regPrice
      regState := sFinished
    }
    is (sFinished) {
      // Expose result
      io.benefitOut.valid := true.B
      io.benefitOut.bits := regBenefit
      when (io.benefitOut.fire) {
        regState := sIdle
      }
    }
  }
}

