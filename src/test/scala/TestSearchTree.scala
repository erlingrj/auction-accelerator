package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import fpgatidbits.dma._


class TestSearchTree extends FlatSpec with ChiselScalatestTester with Matchers {

  val ap = new SearchTaskParams(
    nPEs = 4, bitWidth = 8, maxProblemSize = 16
  )



  def enqueueRound(c: SearchTask, benefits: Seq[Int], ids: Seq[Int], oldPrices: Seq[Int], tLast: Boolean): Unit = {
    // wait for ready
    var ready = false
    while (ready == false) {
      c.clock.step()
      ready = c.io.benefitIn(0).ready.peek().litToBoolean
    }

    // enqueue the round
    (0 until ids.length).map((i) => fork {
      c.io.benefitIn(i).enqueueNow(chiselTypeOf(c.io.benefitIn(i)).bits.Lit(_.id -> ids(i).U, _.benefit -> benefits(i).U, _.oldPrice -> oldPrices(i).U, _.last -> tLast.B))
    }).map(_.join())
  }

  behavior of "SearchTaskPar"
  it should "Initialize correctly" in {
    test(new SearchTask(ap)) { c =>
      c.io.resultOut.ready.poke(true.B)
      c.io.benefitIn.map(_.ready.expect(true.B))
      c.io.resultOut.valid.expect(false.B)
    }
  }

  it should "Find highest value" in {
    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 1.U, _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 2.U, _.oldPrice->20.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 100.U, _.oldPrice->30.U,  _.last->true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 99.U,  _.oldPrice->40.U, _.last->true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 31.U)
        )
      }.join()
    }
  }

  it should "Find highest value with tie" in {
    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 5.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 10.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 10.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 1.U, _.bid -> 11.U)
        )
      }.join()
    }
  }

  it should "Find highest value over multiple rounds" in {

    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 4.U,  _.oldPrice->10.U, _.last->false.B))
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 4.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 10.U,  _.oldPrice->10.U, _.last->false.B))
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 5.U, _.benefit -> 8.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 7.U,  _.oldPrice->10.U, _.last->false.B))
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 6.U, _.benefit -> 20.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 7.U,  _.oldPrice->10.U, _.last->false.B))
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 7.U, _.benefit -> 9.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 6.U, _.bid -> 20.U)
        )
      }.join()

    }
  }
  it should "correct bid on all zeros input" in {
    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 1.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 2.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 3.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 0.U, _.bid -> 0.U)
        )
      }.join()
    }
  }

  it should "Find highest value with zero benefit" in {
    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 3.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(1)).bits.Lit(_.id -> 1.U, _.benefit -> 4.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(2)).bits.Lit(_.id -> 2.U, _.benefit -> 0.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(3)).bits.Lit(_.id -> 3.U, _.benefit -> 2.U,  _.oldPrice->10.U, _.last->true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 1.U, _.bid -> 11.U)
        )
      }.join()
    }
  }
  it should "solve two consecutive problems" in {
    test(new SearchTask(ap)) { c =>
      c.io.benefitIn.map(_.initSource().setSourceClock(c.clock))
      c.io.resultOut.initSink().setSinkClock(c.clock)
      fork {
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 2.U, _.oldPrice -> 10.U, _.last -> true.B))
        c.io.benefitIn(0).enqueueNow(chiselTypeOf(c.io.benefitIn(0)).bits.Lit(_.id -> 0.U, _.benefit -> 3.U, _.oldPrice -> 10.U, _.last -> true.B))
      }.fork {
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(1)).bits.Lit(_.id -> 1.U, _.benefit -> 3.U, _.oldPrice -> 10.U, _.last -> true.B))
        c.io.benefitIn(1).enqueueNow(chiselTypeOf(c.io.benefitIn(1)).bits.Lit(_.id -> 1.U, _.benefit -> 4.U, _.oldPrice -> 10.U, _.last -> true.B))
      }.fork {
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(2)).bits.Lit(_.id -> 2.U, _.benefit -> 4.U, _.oldPrice -> 10.U, _.last -> true.B))
        c.io.benefitIn(2).enqueueNow(chiselTypeOf(c.io.benefitIn(2)).bits.Lit(_.id -> 2.U, _.benefit -> 0.U, _.oldPrice -> 10.U, _.last -> true.B))
      }.fork {
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(3)).bits.Lit(_.id -> 3.U, _.benefit -> 1.U, _.oldPrice -> 10.U, _.last -> true.B))
        c.io.benefitIn(3).enqueueNow(chiselTypeOf(c.io.benefitIn(3)).bits.Lit(_.id -> 3.U, _.benefit -> 2.U, _.oldPrice -> 10.U, _.last -> true.B))
      }.fork {
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 2.U, _.bid -> 11.U)
        )
        c.io.resultOut.expectDequeue(
          chiselTypeOf(c.io.resultOut)
            .bits.Lit(_.winner -> 1.U, _.bid -> 11.U)
        )
      }.join()
    }
  }
}
