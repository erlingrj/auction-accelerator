package auction

import chisel3._
import chisel3.util._

import fpgatidbits.synthutils.PrintableParam


class PEExtPriceRewardIO(private val ap: ProcessingElementParams) extends Bundle {
  val reward = UInt(ap.bitWidth.W)
  val idx = UInt(ap.agentWidth.W)
  val last = Bool()
}

class ProcessingElementExtPriceIO(ap: ProcessingElementParams) extends Bundle {

  val priceStore = new RegStoreTransaction(0.U(ap.bitWidth.W),ap.priceRegStoreParams)
  val rewardIn = Flipped(Decoupled(new PEExtPriceRewardIO(ap)))
  val stP = new SearchTaskParams(
    bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs
  )
  val PEResultOut = Decoupled(new PEResult(stP))

  val accountantNotifyContinue = Input(Bool())

  def driveDefaults() = {
    PEResultOut.valid := false.B
    PEResultOut.bits := DontCare
    priceStore.req.valid:= false.B
    priceStore.rsp.ready := false.B
    priceStore.req.bits := DontCare
    rewardIn.ready := false.B
  }
}

class ProcessingElementExtPrice(ap: ProcessingElementParams) extends MultiIOModule {
  val io = IO(new ProcessingElementExtPriceIO(ap))

  val sIdle :: sProcess  :: sFinished :: sStall :: Nil = Enum(4)
  val regState = RegInit(sIdle)
  val regReward = RegInit(0.U(ap.bitWidth.W))
  val regIdx = RegInit(0.U(ap.agentWidth.W))
  val regBenefit = RegInit(0.U(ap.bitWidth.W))
  val regLast = RegInit(false.B)
  val regPrice = RegInit(0.U(ap.bitWidth.W))
  // Drive signals to default
  io.driveDefaults()


  switch (regState) {
    is (sIdle) {
      // Idle state. We wait for valid input on both rewardIn and priceIn
      io.rewardIn.ready := true.B

      when(io.rewardIn.valid) {
        when (io.rewardIn.bits.reward === 0.U) {
          assert(io.rewardIn.bits.last)
          regState := sStall
        }.otherwise {
          val idx = WireInit(io.rewardIn.bits.idx)
          regIdx := idx

          // Fetch price
          io.priceStore.req.valid := true.B
          io.priceStore.req.bits.wen:= false.B
          io.priceStore.req.bits.addr := idx
          io.priceStore.rsp.ready := true.B
          regPrice := io.priceStore.rsp.bits.rdata

          regReward := io.rewardIn.bits.reward
          regState := sProcess
          regLast := io.rewardIn.bits.last
        }
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
      io.PEResultOut.bits.id := regIdx
      io.PEResultOut.bits.last := regLast
      when(io.PEResultOut.fire()) {
        when (regLast) {
          regState := sStall
        }.otherwise{
          regState := sIdle
          regPrice := 0.U
          regIdx := 0.U
          regBenefit := 0.U
          regReward := 0.U
          regLast := false.B
        }
      }
    }
    is (sStall) {
      when (io.accountantNotifyContinue) {
        regState := sIdle
        regPrice := 0.U
        regIdx := 0.U
        regBenefit := 0.U
        regReward := 0.U
        regLast := false.B
      }
    }
  }
}
