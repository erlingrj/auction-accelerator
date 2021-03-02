package auction

import chisel3._
import chisel3.util._



// ProcessingElements do the processing (subtraction) and calculates the net benefit
// ProcessingElementPar is the "paralell" implementation

class PEControl(private val ap: AuctionParams) extends Bundle {
  val prices = Vec(ap.maxProblemSize/ap.nPEs, UInt(ap.bitWidth.W))
}

class PERewardIO(private val ap: AuctionParams) extends Bundle {
  val reward = UInt(ap.bitWidth.W)
  val last = Bool()
}

class ProcessingElementParIO(ap: AuctionParams) extends Bundle {

  val controlIn = Flipped(Decoupled(new PEControl(ap)))
  val rewardIn = Flipped(Decoupled(new PERewardIO(ap)))
  val PEResultOut = Decoupled(new PEResult(ap))

  def driveDefaults() = {
    PEResultOut.valid := false.B
    PEResultOut.bits := DontCare
    controlIn.ready := false.B
    rewardIn.ready := false.B
  }
}

class ProcessingElementPar(ap: AuctionParams, id: Int) extends MultiIOModule {
  val io = IO(new ProcessingElementParIO(ap))

  def getId(idx: UInt): UInt = {
    idx * ap.nPEs.U + id.U
  }

  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)

  val regReward = RegInit(0.U(ap.bitWidth.W))
  val regPrice = RegInit(0.U(ap.bitWidth.W))
  val regIdx = RegInit(0.U(ap.agentWidth.W))
  val regBenefit = RegInit(0.U(ap.bitWidth.W))
  val regLast = RegInit(false.B)


  // Drive signals to default
  io.driveDefaults()


  switch (regState) {
    is (sIdle) {

      io.controlIn.ready := true.B
      io.rewardIn.ready := true.B

      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.controlIn.valid) {
        assert(regIdx < (ap.maxProblemSize/ap.nPEs).U)
        regReward := io.rewardIn.bits.reward
        regPrice := io.controlIn.bits.prices(regIdx)
        regState := sProcess
        regLast := io.rewardIn.bits.last

      }
    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later
      printf("process\n")
      val diff = regReward.zext() - regPrice.zext()
      regBenefit := Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))
      regState := sFinished
    }
    is (sFinished) {
      printf("finish\n")
      // Expose result
      io.PEResultOut.valid := true.B
      io.PEResultOut.bits.benefit := regBenefit
      io.PEResultOut.bits.id := getId(regIdx)
      io.PEResultOut.bits.last := regLast
      when (io.PEResultOut.fire) {
        when (regLast) {
          regIdx := 0.U
        } otherwise {
          regIdx := regIdx + 1.U
        }
        regState := sIdle
      }
    }
  }
}
