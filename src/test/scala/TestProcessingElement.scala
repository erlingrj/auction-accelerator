package auction

import org.scalatest._
import chiseltest._
import chisel3._

class TestProcessingElement extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
    val memWidth = 32
    val maxProblemSize = 8
  }

  behavior of "ProcessingElement"
  it should "Initialize correctly" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      // c.regState.expect(c.sIdle)
      // c.regReward.expect(0.U)
      // c.regPrice.expect(0.U)
      // c.regBenefit.expect(0.U)

      c.io.benefitOut.valid.expect(false.B)
      c.io.rewardIn.ready.expect(false.B)
      c.io.priceIn.ready.expect(false.B)
    }
  }
  it should "Calculate single benefit" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.priceIn.enqueue(4.U)
      }.fork {
        c.io.rewardIn.enqueue(8.U)
      }.fork {
        c.io.benefitOut.expectDequeue(4.U)
      }.join()
    }
  }

  it should "Handle negative benefit" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      fork {
        c.io.priceIn.enqueue(8.U)
      }.fork {
        c.io.rewardIn.enqueue(4.U)
      }.fork {
        c.io.benefitOut.expectDequeue("hffff_fffc".U)
      }.join()
    }
  }


  it should "Handle a stream of input data" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      c.io.priceIn.bits.poke(100.U)
      c.io.priceIn.valid.poke(true.B)
      fork {
        c.io.rewardIn.enqueueSeq(Seq.tabulate(100)(i => (i+100).U))
      }.fork {
        c.io.benefitOut.expectDequeueSeq(Seq.tabulate(100)(i => (i.U)))
      }.join()
    }
  }

  it should "handle stalling on output interface" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      val price = 45
      c.io.priceIn.bits.poke(price.U)
      c.io.priceIn.valid.poke(true.B)
      val it = 100
      val rewardIn = Seq.tabulate(it)(i => (45+i))

      fork {
        c.io.rewardIn.enqueueSeq(rewardIn.map(_.U))
      }.fork {
        for (i <- 0 until it) {
          if (i%4 == 0) {
            c.clock.step(50)
          }
          c.io.benefitOut.expectDequeue((rewardIn(i)-price).U)
        }
      }.join()
    }
  }

  it should "Handle stalling on input interface" in {
    test(new ProcessingElement(AuctionTestParams)) {c =>
      c.io.priceIn.initSource.setSourceClock(c.clock)
      c.io.rewardIn.initSource.setSourceClock(c.clock)
      c.io.benefitOut.initSink().setSinkClock(c.clock)

      val it = 100
      val priceIn = 69
      val rewardIn = Seq.tabulate(it)(i => (i+69))
      c.io.priceIn.valid.poke(true.B)
      c.io.priceIn.bits.poke(priceIn.U)

      fork {
        for (i <- 0 until it) {
          if (i%4 == 0) {
            c.clock.step(50)
          }
          c.io.rewardIn.enqueue(rewardIn(i).U)
        }
      }.fork {
        c.io.benefitOut.expectDequeueSeq(rewardIn.map( a => (a - 69).U))
      }.join()
    }
  }
}

class TestPEsToSearchTask extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
    val memWidth = 32
    val maxProblemSize = 8
  }
  behavior of "PEsToSearchTask"

  it should "Pass out correctly" in {
    test(new PEsToSearchTask(AuctionTestParams)) {c =>
      c.peIn.map(_.initSource.setSourceClock(c.clock))
      c.searchOut.initSink.setSinkClock(c.clock)

      fork {
        c.peIn(0).enqueueSeq(Seq(1.U,2.U))
      }.fork {
        c.peIn(1).enqueueSeq(Seq(3.U,4.U))
      }.fork {
        c.peIn(2).enqueueSeq(Seq(5.U,6.U))
      }.fork {
        c.peIn(3).enqueueSeq(Seq(7.U,8.U))
      }.fork {
        c.searchOut.expectDequeueSeq(
          Seq(1.U, 3.U, 5.U, 7.U, 2.U, 4.U, 6.U, 8.U)
        )
      }.join()
    }
  }
}
