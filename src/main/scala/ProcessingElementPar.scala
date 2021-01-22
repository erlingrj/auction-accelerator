package auction

import chisel3._
import chisel3.util._



// ProcessingElements do the processing (subtraction) and calculates the net benefit
// ProcessingElementPar is the "paralell" implementation

class PEControl(private val ap: AuctionParams) extends Bundle {
  val price = UInt(ap.bitWidth.W)
  val id = UInt(ap.agentWidth.W)
}

class ProcessingElementParIO(ap: AuctionParams) extends Bundle {

  val controlIn = Flipped(Decoupled(new PEControl(ap)))
  val rewardIn = Flipped(Decoupled(UInt(ap.bitWidth.W)))
  val PEResultOut = Decoupled(new PEResult(ap))

  def driveDefaults() = {
    PEResultOut.valid := false.B
    PEResultOut.bits := DontCare
    controlIn.ready := false.B
    rewardIn.ready := false.B
  }
}

class ProcessingElementPar(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new ProcessingElementParIO(ap))

  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regReward = RegInit(0.U(ap.bitWidth.W))
  val regPrice = RegInit(0.U(ap.bitWidth.W))
  val regId = RegInit(0.U(ap.agentWidth.W))
  val regBenefit = RegInit(0.U(ap.bitWidth.W))


  // Drive signals to default
  io.driveDefaults()


  switch (regState) {
    is (sIdle) {
      io.controlIn.ready := true.B
      io.rewardIn.ready := true.B

      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.controlIn.valid) {
        regReward := io.rewardIn.bits
        regPrice := io.controlIn.bits.price
        regId := io.controlIn.bits.id
        regState := sProcess
      }
    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later

      val diff = regReward.zext() - regPrice.zext()
      regBenefit := Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))
      regState := sFinished
    }
    is (sFinished) {
      // Expose result
      io.PEResultOut.valid := true.B
      io.PEResultOut.bits.benefit := regBenefit
      io.PEResultOut.bits.id := regId
      when (io.PEResultOut.fire) {
        regState := sIdle
      }
    }
  }
}
