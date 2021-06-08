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

class PERewardIO(private val ap: ProcessingElementParams) extends Bundle {
  val reward = UInt(ap.bitWidth.W)
  val idx = UInt(ap.agentWidth.W)
  val last = Bool()
}

class ProcessingElementIO(ap: ProcessingElementParams) extends Bundle {

  val priceStore = new RegStoreTransaction(0.U(ap.bitWidth.W),ap.priceRegStoreParams)
  val rewardIn = Flipped(Decoupled(new PERewardIO(ap)))
  val stP = new SearchTreeParams(
    bitWidth = ap.bitWidth, maxProblemSize = ap.maxProblemSize, nPEs = ap.nPEs
  )

  val accountantNotify = Input(Bool())

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

class ProcessingElement(ap: ProcessingElementParams) extends MultiIOModule {
  val io = IO(new ProcessingElementIO(ap))

  val sNormal :: sStall :: Nil = Enum(2)

  val regState = RegInit(sNormal)

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
  val stall = WireInit(!io.PEResultOut.ready)

  when(!stall) {
    // Stage 1
    io.rewardIn.ready := true.B
    val fire = io.rewardIn.fire()
    s1_valid := fire
    when (fire) {
      val price = io.priceStore.read(io.rewardIn.bits.idx)
      val idx = io.rewardIn.bits.idx
      val last = io.rewardIn.bits.last
      val reward = io.rewardIn.bits.reward
      val diff = reward.zext() - price.zext()
      val benefit =  Mux(diff(ap.bitWidth) === 1.U, 0.U, diff(ap.bitWidth-1, 0))

      io.PEResultOut.valid := true.B
      io.PEResultOut.bits.last := last
      io.PEResultOut.bits.id := idx
      io.PEResultOut.bits.benefit := benefit
      io.PEResultOut.bits.oldPrice := price
    }
  }


  switch (regState) {
    is (sStall) {
      stall := true.B
      when (io.accountantNotify) {
        regState := sNormal
      }
    }
    is (sNormal) {
      when (io.rewardIn.fire() && io.rewardIn.bits.last) {
        regState := sStall
      }
    }
  }
}
