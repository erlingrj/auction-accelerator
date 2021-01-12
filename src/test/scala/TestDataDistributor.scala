package auction

import org.scalatest._
import chiseltest._
import chisel3._

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
