package auction

import chisel3._
import chisel3.util._

import fpgatidbits.synthutils.PrintableParam

class ProcessingElementParams(
  val bitWidth: Int,
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
// ProcessingElements do the processing (subtraction) and calculates the net benefit
// ProcessingElementPar is the "paralell" implementation

class PEControl(private val ap: ProcessingElementParams) extends Bundle {
  val prices = Vec(ap.maxProblemSize/ap.nPEs, UInt(ap.bitWidth.W))
}

class PERewardIO(private val ap: ProcessingElementParams) extends Bundle {
  val reward = UInt(ap.bitWidth.W)
  val last = Bool()
}

class ProcessingElementParIO(ap: ProcessingElementParams) extends Bundle {

  val controlIn = Flipped(Decoupled(new PEControl(ap)))
  val rewardIn = Flipped(Decoupled(new PERewardIO(ap)))
  val stP = new SearchTaskParams(
    bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs
  )
  val PEResultOut = Decoupled(new PEResult(stP))

  def driveDefaults() = {
    PEResultOut.valid := false.B
    PEResultOut.bits := DontCare
    controlIn.ready := false.B
    rewardIn.ready := false.B
  }
}

class ProcessingElementPar(ap: ProcessingElementParams, id: Int) extends MultiIOModule {
  val io = IO(new ProcessingElementParIO(ap))

  def getId(idx: UInt): UInt = {
    idx * ap.nPEs.U + id.U
  }
  val constPricePerPE = ap.maxProblemSize/ap.nPEs

  val sIdle :: sProcess :: sIterate :: sFinished :: Nil = Enum(4)
  val regState = RegInit(sIdle)
  val regReward = RegInit(0.U(ap.bitWidth.W))
  val regPrices = RegInit(VecInit(Seq.fill(constPricePerPE)(0.U(ap.bitWidth.W))))
  val regIdx = RegInit(0.U(ap.agentWidth.W))
  val regBenefit = RegInit(0.U(ap.bitWidth.W))
  val regLast = RegInit(false.B)

  // Drive signals to default
  io.driveDefaults()


  switch (regState) {
    is (sIdle) {
      // Idle state. We wait for valid input on both rewardIn and priceIn
      when(io.rewardIn.valid && io.controlIn.valid) {
        io.controlIn.ready := true.B
        io.rewardIn.ready := true.B

        assert(regIdx < (ap.maxProblemSize / ap.nPEs).U)
        regReward := io.rewardIn.bits.reward
        regPrices := io.controlIn.bits.prices
        regState := sProcess
        regLast := io.rewardIn.bits.last
      }
    }
    is (sProcess) {
      // We do calculation (subtraction) beware that we might get negative reward so check msb later
      val diff = regReward.zext() - regPrices(regIdx).zext()
      regBenefit := Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))
      regState := sFinished
    }

    is (sFinished) {
      // Expose result
      io.PEResultOut.valid := true.B
      io.PEResultOut.bits.benefit := regBenefit
      io.PEResultOut.bits.id := getId(regIdx)
      io.PEResultOut.bits.last := regLast
      when(io.PEResultOut.fire()) {
        when(regLast) {
          regIdx := 0.U
          regState := sIdle
        } otherwise {
          regIdx := regIdx + 1.U
          regState := sIterate
        }
      }
    }
    is (sIterate) {
      io.rewardIn.ready := true.B
      when (io.rewardIn.valid) {
        assert(regIdx < (ap.maxProblemSize / ap.nPEs).U)
        regReward := io.rewardIn.bits.reward
        regState := sProcess
        regLast := io.rewardIn.bits.last
      }.elsewhen(io.rewardIn.bits.last) {
        regIdx := 0.U
        regState := sIdle
    }
  }
}
}
