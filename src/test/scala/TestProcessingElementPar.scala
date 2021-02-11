package auction


import chisel3.experimental.BundleLiterals._
import org.scalatest._
import chiseltest._
import chisel3._

class TestProcessingElementPar extends FlatSpec with ChiselScalatestTester with Matchers {


  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
    val memWidth = 32
    val maxProblemSize = 8
  }

  behavior of "ProcessingElementPar"
  it should "Initialize correctly" in {
    test(new ProcessingElementPar(AuctionTestParams)) { c =>
      // c.regState.expect(c.sIdle)
      // c.regReward.expect(0.U)
      // c.regPrice.expect(0.U)
      // c.regBenefit.expect(0.U)


      c.io.PEResultOut.valid.expect(false.B)
      c.io.rewardIn.ready.expect(true.B)

      c.io.controlIn.ready.expect(true.B)
    }
  }

  it should "Calculate single benefit" in {
    test(new ProcessingElementPar(AuctionTestParams)) { c =>
      c.io.controlIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.PEResultOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.controlIn.enqueue(chiselTypeOf(c.io.controlIn).bits.Lit(_.prices -> VecInit(Seq(5.U))))
      }.fork {
        c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 8.U, _.id ->0.U, _.last -> true.B))
      }.fork {
        c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 3.U, _.id -> 0.U, _.last->true.B))
      }.join()
    }
  }

  it should "Handle negative benefit" in {
    test(new ProcessingElementPar(AuctionTestParams)) { c =>
      c.io.controlIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.PEResultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.controlIn.enqueue(chiselTypeOf(c.io.controlIn).bits.Lit(_.prices -> VecInit(Seq(16.U))))
      }.fork {
        c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 8.U, _.id -> 0.U, _.last -> false.B))
      }.fork {
        c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 0.U, _.id -> 0.U, _.last -> false.B))
      }.join()
    }
  }

}

