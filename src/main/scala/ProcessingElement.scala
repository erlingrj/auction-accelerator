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
}

class PERewardIO(private val ap: ProcessingElementParams) extends Bundle {
  val reward = UInt(ap.bitWidth.W)
  val idx = UInt(ap.agentWidth.W)
  val last = Bool()
}

class ProcessingElementIO(ap: ProcessingElementParams) extends Bundle {

  val price = Input(UInt(ap.bitWidth.W))
  val agentIdx = Output(UInt(ap.agentWidth.W))
  val agentIdxReqValid = Output(Bool())

  val rewardIn = Flipped(Decoupled(new PERewardIO(ap)))
  val stP = new SearchTreeParams(
    bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs
  )
  val PEResultOut = Decoupled(new PEResult(stP))

  def driveDefaults() = {
    PEResultOut.valid := false.B
    PEResultOut.bits := DontCare
    rewardIn.ready := false.B
    agentIdx := DontCare
    agentIdxReqValid := false.B
  }
}

class ProcessingElement(ap: ProcessingElementParams) extends MultiIOModule {
  val io = IO(new ProcessingElementIO(ap))

  val sIdle :: sProcess  :: sFinished :: sStall :: Nil = Enum(4)


  val s1_last = RegInit(0.U(ap.bitWidth.W))
  val s1_reward = RegInit(0.U(ap.bitWidth.W))
  val s1_idx = RegInit(0.U(ap.bitWidth.W))
  val s1_valid = RegInit(false.B)

  val s2_price = RegInit(0.U(ap.bitWidth.W))
  val s2_idx = RegInit(0.U(ap.bitWidth.W))
  val s2_last = RegInit(false.B)
  val s2_valid = RegInit(false.B)
  val s2_reward = RegInit(0.U(ap.bitWidth.W))

  val s3_benefit = RegInit(0.U(ap.bitWidth.W))
  val s3_oldPrice = RegInit(0.U(ap.bitWidth.W))
  val s3_valid = RegInit(false.B)
  val s3_last = RegInit(0.U(ap.bitWidth.W))
  val s3_idx = RegInit(0.U(ap.bitWidth.W))

  // Drive signals to default
  io.driveDefaults()
  val stall = !io.PEResultOut.ready

  when(!stall) {
    // Stage 1
    io.rewardIn.ready := true.B
    val fire = io.rewardIn.fire()
    s1_valid := fire
    when (fire) {
      io.agentIdx := io.rewardIn.bits.idx
      io.agentIdxReqValid := true.B
      s1_idx := io.rewardIn.bits.idx
      s1_last := io.rewardIn.bits.last
      s1_reward := io.rewardIn.bits.reward
    }

    // stage 2
    s2_valid := s1_valid
    s2_idx := s1_idx
    s2_last := s1_last
    s2_reward := s1_reward
    s2_price := io.price

    // stage 3
    val diff = s2_reward.zext() - s2_price.zext()
    s3_benefit :=  Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))
    s3_oldPrice := s2_price
    s3_valid := s2_valid
    s3_last := s2_last
    s3_idx := s2_idx
  }

  io.PEResultOut.valid := s3_valid
  io.PEResultOut.bits.last := s3_last
  io.PEResultOut.bits.id := s3_idx
  io.PEResultOut.bits.benefit := s3_benefit
  io.PEResultOut.bits.oldPrice := s3_oldPrice

}