package auction

import chisel3._
import chisel3.util._

import fpgatidbits.synthutils.PrintableParam

class ProcessingElementParams(
  val bitWidth: Int,
  val nPEs: Int,
  val maxProblemSize: Int,
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
  def priceRegStoreParams: RegStoreParams = new RegStoreParams(nPEs,0 ,1,agentWidth)
}

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



  val s1_price = RegInit(0.U(ap.bitWidth.W))
  val s1_last = RegInit(0.U(ap.bitWidth.W))
  val s1_reward = RegInit(0.U(ap.bitWidth.W))
  val s1_idx = RegInit(0.U(ap.bitWidth.W))
  val s1_valid = RegInit(false.B)

  val s2_benefit = RegInit(0.U(ap.bitWidth.W))
  val s2_oldPrice = RegInit(0.U(ap.bitWidth.W))
  val s2_idx = RegInit(0.U(ap.bitWidth.W))
  val s2_last = RegInit(false.B)
  val s2_valid = RegInit(false.B)


  // Drive signals to default
  io.driveDefaults()
  val stall = !io.PEResultOut.ready

  when(!stall) {
    // Stage 1
    io.rewardIn.ready := true.B
    val fire = io.rewardIn.fire()
    s1_valid := fire
    when (fire) {
      s1_price := io.priceStore.read(io.rewardIn.bits.idx)
      s1_idx := io.rewardIn.bits.idx
      s1_last := io.rewardIn.bits.last
      s1_reward := io.rewardIn.bits.reward
    }

    // stage 2
    s2_valid := s1_valid
    s2_idx := s1_idx
    s2_last := s1_last
    val diff = s1_reward.zext() - s1_price.zext()
    s2_benefit :=  Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))
    s2_oldPrice := s1_price

  }

  io.PEResultOut.valid := s2_valid
  io.PEResultOut.bits.last := s2_last
  io.PEResultOut.bits.id := s2_idx
  io.PEResultOut.bits.benefit := s2_benefit
  io.PEResultOut.bits.oldPrice := s2_oldPrice

}
