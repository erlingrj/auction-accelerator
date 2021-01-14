package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._

class TestSearchTask extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
  }

  behavior of "SearchTask"
  it should "Initialize correctly" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.ready.expect(true.B)
      c.io.resultOut.valid.expect(false.B)
    }
  }
  it should "Find highest value" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq(1.U, 3.U,5.U,4.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 1.U)
        )
      }.join()
    }
  }

  it should "find highest value in stream" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)

      val inputStream =
        Seq(
          10.U, 0.U, 3.U, 4.U,
          2.U, 3.U, 10.U, 2.U,
          69.U, 10.U, 3.U, 42.U
        )
      fork {
        c.io.benefitIn.enqueueSeq(inputStream)
      }.fork {
        c.io.resultOut.expectDequeueSeq(Seq(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 6.U),
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 7.U),
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 27.U)
        )
        )
      }.join()
    }
  }


  it should "accept negative benefit" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq("hf000_0001".U, 3.U,5.U,10.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 3.U, _.bid -> 5.U)
        )
      }.join()
    }
  }

  it should "correct bid on all zeros input" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.benefitIn.enqueueSeq(Seq(0.U, 0.U,0.U,0.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 0.U)
        )
      }.join()
    }
  }

  it should "Give the right bid for second highest negative" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq(10.U, "hf000_1234".U,"hffff_ffff".U,"hf123_4321".U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 10.U)
        )
      }.join()
    }
  }
  it should "Work for all tied zeros" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq(0.U, 0.U,0.U,0.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 0.U)
        )
      }.join()
    }
  }

  it should "Give correct bid on all negative inputs" in {
    test(new SearchTask(AuctionTestParams)) { c =>
      c.io.benefitIn.initSource().setSourceClock(c.clock)
      c.io.resultOut.initSink().setSinkClock(c.clock)


      fork {
        c.io.benefitIn.enqueueSeq(Seq("hffff_0000".U, "hffff_0000".U,"hffff_0000".U,"hffff_0000".U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 0.U)
        )
      }.join()
    }
  }
}
