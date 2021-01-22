package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._

class TestSearchTaskPar extends FlatSpec with ChiselScalatestTester with Matchers {


  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
    val memWidth = 32
    val maxProblemSize = 8
  }


  def enqueueRound(c: SearchTaskPar, benefits: Seq[Int], ids: Seq[Int], tLast: Boolean): Unit = {
    // wait for ready
    var ready = false
    while (ready == false) {
      c.clock.step()
      ready = c.io.benefitIn(0).ready.peek().litToBoolean
    }

    // enqueue the round
    fork {
      c.io.tLast.poke(tLast.B)
      c.clock.step()
      c.io.tLast.poke(false.B)
    }.fork {
      for (i <- 0 until ids.length) {
        c.io.benefitIn(i).enqueue(chiselTypeOf(c.io.benefitIn(i)).bits.Lit(_.id -> ids(i).U, _.benefit -> benefits(i).U))
      }
    }.join()
  }

  behavior of "SearchTaskPar"
  it should "Initialize correctly" in {
    test(new SearchTaskPar(AuctionTestParams)) { c =>
      c.io.benefitIn.map(_.ready.expect(true.B))
      c.io.resultOut.valid.expect(false.B)
    }
  }

  it should "Find highest value" in {
    test(new SearchTaskPar(AuctionTestParams)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.tLast.poke(true.B)
        c.clock.step()
        c.io.tLast.poke(false.B)
      }.fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 1.U))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 2.U))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 100.U))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 99.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 1.U)
        )
      }.join()
    }
  }

  it should "Find highest value with tie" in {
    test(new SearchTaskPar(AuctionTestParams)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.tLast.poke(true.B)
        c.clock.step()
        c.io.tLast.poke(false.B)
      }.fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 5.U))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 10.U))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 10.U))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 0.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 1.U, _.bid -> 1.U)
        )
      }.join()
    }
  }

  it should "Find highest value over multiple rounds" in {

    test(new SearchTaskPar(AuctionTestParams)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.tLast.poke(false.B)
        c.clock.step()
        c.io.tLast.poke(false.B)
      }.fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 4.U))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 10.U))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 7.U))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 7.U))
      }.fork {
        var ready = false
        while (ready == false) {
          c.clock.step()
          ready = c.io.benefitIn(0).ready.peek().litToBoolean
        }
      }.join()

      // Round 2

      fork {
        c.io.tLast.poke(true.B)
        c.clock.step()
        c.io.tLast.poke(false.B)
      }.fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 4.U, _.benefit -> 0.U))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 5.U, _.benefit -> 8.U))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 6.U, _.benefit -> 20.U))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 7.U, _.benefit -> 9.U))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 6.U, _.bid -> 10.U)
        )
      }.join()
    }
  }
  it should "correct bid on all zeros input" in {
    test(new SearchTaskPar(AuctionTestParams)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)

      val bids = Seq(0,0,0,0)
      val ids = Seq(0,1,2,3)
      val tlast = true

      enqueueRound(c, bids, ids, tlast)

      c.io.resultOut.expectDequeue(
        chiselTypeOf(c.io.resultOut)
          .bits.Lit(_.winner -> 0.U, _.bid ->0.U)
      )

    }
  }
}