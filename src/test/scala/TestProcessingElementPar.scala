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
    test(new ProcessingElementPar(AuctionTestParams, 0)) { c =>
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
    test(new ProcessingElementPar(AuctionTestParams, 0)) { c =>
    //  c.io.controlIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.PEResultOut.initSink().setSinkClock(c.clock)

      c.io.controlIn.valid.poke(true.B)
      c.io.controlIn.bits.prices(0).poke(5.U)
      c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 8.U, _.last -> true.B))
      c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 3.U, _.id -> 0.U, _.last->true.B))
    }
  }

  it should "Handle negative benefit" in {
    test(new ProcessingElementPar(AuctionTestParams, 0)) { c =>
      c.io.controlIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.PEResultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.controlIn.valid.poke(true.B)
        c.io.controlIn.bits.prices(0).poke(16.U)
        c.clock.step(1)
      }.fork {
        c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 8.U,  _.last -> false.B))
      }.fork {
        c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 0.U, _.id -> 0.U, _.last -> false.B))
      }.join()
    }
  }

  it should "Handle multiple rounds" in {
    test(new ProcessingElementPar(AuctionTestParams, 3)) { c =>
      c.io.controlIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.PEResultOut.initSink().setSinkClock(c.clock)
      fork {
        c.clock.step(1)
      }.fork {
        c.io.controlIn.valid.poke(true.B)
        c.io.controlIn.bits.prices(0).poke(4.U)
        c.io.controlIn.bits.prices(1).poke(5.U)
        c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 5.U,  _.last -> false.B))
        c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(_.reward -> 10.U,  _.last -> true.B))
      }.fork {
        c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 1.U, _.id -> 3.U, _.last -> false.B))
        c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(_.benefit -> 5.U, _.id -> 7.U, _.last -> true.B))
      }.join()
    }
  }
}

