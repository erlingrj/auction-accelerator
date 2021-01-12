package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import fpgatidbits.PlatformWrapper._
import chisel3.util._


import fpgatidbits.PlatformWrapper.GenericAccelImplicits._


class TestAuction extends FlatSpec with ChiselScalatestTester with Matchers {
  object Ap extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }

  behavior of "AuctionAlgo"

  it should "work on simple 4x4 example" in {
    test(new TesterWrapper({p => new Auction(p)}, "_dump")) { c =>
      c.writeReg("nRows", 4.U)
      c.writeReg("nCols", 4.U)
      c.writeReg("baseAddr", 0.U)

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0,rewardArr)
      c.writeReg("start", 1.U)

      var cnt = 0
      while (c.readReg("finished").litValue != 1 &&
        cnt < 100) {

          c.clock.step()
          cnt = cnt + 1
        }
        c.expectReg("finished", 1.U)
      }
    }


  it should "read/write multiple matrix to memory" in {
    test(new TesterWrapper({p => new Auction(p)}, "_dump")) { c =>

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0,rewardArr)
      var addr = 0
      for (v <- rewardArr) {
        c.expectMem(addr, v)
        addr = addr+8
      }
    }
  }



  it should "Read/Write CSR" in {
    test(new TesterWrapper({p => new Auction(p)}, "_dump")) { c =>

      c.writeReg("nRows", 4.U)
      c.expectReg("nRows", 4.U)
    }
  }
  it should "read/write to mem" in {
    test(new TesterWrapper({p => new Auction(p)}, "_dump")) { c =>
      c.writeMem(8, 69.U)
      c.expectMem(8, 69.U)
    }
  }


}




class TestPEsToSearchTask extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
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

class TestAuctionController extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }


  behavior of "AuctionController"

  it should "Initialize correctly" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.finished.expect(false.B)
      c.io.streamReaderCtrlSignals.start.expect(false.B)
    }
  }

  it should "Request the correct memory addresses" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.baseAddress.poke(0.U)
      c.io.nCols.poke(4.U)
      c.io.nRows.poke(4.U)

      c.clock.step(4)
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(0.U)

    }
  }


  it should "Request and update correctly" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.searchResultIn.initSource.setSourceClock(c.clock)
      c.io.baseAddress.poke(0.U)
      c.io.nCols.poke(4.U)
      c.io.nRows.poke(4.U)

      c.clock.step(4)
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(0.U)


      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(true.B)
      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(false.B)
      c.io.searchResultIn.ready.expect(true.B)

      c.io.searchResultIn.enqueueNow(
        chiselTypeOf(c.io.searchResultIn)
          .bits.Lit(_.winner -> 2.U, _.bid -> 69.U)
      )
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(16.U)


      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(true.B)
      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(false.B)
      c.io.searchResultIn.ready.expect(true.B)


      c.io.searchResultIn.enqueueNow(
        chiselTypeOf(c.io.searchResultIn)
          .bits.Lit(_.winner -> 0.U, _.bid -> 169.U)
      )
      c.clock.step(1)
    }
  }

}

class TestSearchTask extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
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


class TestProcessingElement extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
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


class TestDataDistributor extends FlatSpec with ChiselScalatestTester with Matchers {
  object AuctionTestParams extends AuctionParams {
    val nProcessingElements = 4
    val datSz = 32
  }

  behavior of "DataDistributor"
  it should "Initialize read/valid interfaces correctly" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.peOut.map(_.valid.expect(false.B))
      c.mem.valid.poke(false.B)

      c.peOut.map(_.ready.poke(false.B))
      c.mem.ready.expect(false.B)
    }
  }

  it should "Pass simple data through" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.peOut(0).ready.poke(true.B)
      c.mem.valid.poke(true.B)
      c.mem.bits.poke(69.U)

      c.peOut(0).valid.expect(true.B)
      c.peOut(0).bits.expect(69.U)
    }
  }

  it should "Pass a stream of data out correctly" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }

      for (i <- 0 until 100) {
        println(s"i=$i cnt=${c.cnt.peek}")
        c.peOut.zipWithIndex.map({ case (io, idx) =>
          if (idx == i%4) {
            io.expectDequeueNow((i.U))
          }
        })
      }
    }
  }

  it should "Pass a stream of data out correctly 2" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }.fork {
        c.peOut(0).expectDequeueSeq(Seq.tabulate(25)(idx => (idx*4).U))
      }.fork {
        c.peOut(1).expectDequeueSeq(Seq.tabulate(25)(idx => (1+(idx*4)).U))
      }.fork {
        c.peOut(2).expectDequeueSeq(Seq.tabulate(25)(idx => (2+(idx*4)).U))
      }.fork {
        c.peOut(3).expectDequeueSeq(Seq.tabulate(25)(idx => (3+(idx*4)).U))
      }.join()
    }
  }

  it should "Pass a stream of data out correctly with stalls" in {
    test(new DataDistributor(AuctionTestParams)) { c =>
      c.mem.initSource().setSourceClock(c.clock)
      c.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.mem.enqueueSeq(Seq.tabulate(100)(idx => idx.U))
      }.fork {
        c.peOut(0).expectDequeueSeq(Seq.tabulate(25)(idx => (idx*4).U))
      }.fork {
        c.peOut(1).expectDequeueSeq(Seq.tabulate(25)(idx => (1+(idx*4)).U))
      }.fork {
        for (i <- 0 until 25) {
          if (i % 4 == 0) c.clock.step(100)
          c.peOut(2).expectDequeue((2+(i*4)).U)
        }
      }.fork {
        c.peOut(3).expectDequeueSeq(Seq.tabulate(25)(idx => (3+(idx*4)).U))
      }.join()
    }
  }
}