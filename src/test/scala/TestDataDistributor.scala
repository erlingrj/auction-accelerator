package auction

import Chisel.Cat
import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import fpgatidbits.dma.MemReqParams

class TestDataDistributorParUnO extends FlatSpec with ChiselScalatestTester with Matchers {

  val ap1 = new DataDistributorParams(
    nPEs = 1, bitWidth = 32, memWidth = 32, maxProblemSize = 16
  )

  behavior of "DataDistributorParUnO"
  it should "Initialize read/valid interfaces correctly" in {
    test(new DataDistributorParUnO(ap1)) { c =>
      c.io.peOut.map(_.valid.expect(false.B))
      c.io.mem.valid.poke(false.B)
    }
  }

  it should "Pass simple data through" in {
    test(new DataDistributorParUnO(ap1)) { c =>
      c.io.mem.initSource().setSourceClock(c.clock)
      c.io.peOut.map(_.initSink.setSinkClock(c.clock))
      fork {
        c.io.mem.enqueue(
          chiselTypeOf(c.io.mem).bits.Lit(
            _.data -> 69.U,
            _.mask -> 1.U,
            _.last -> false.B
          )
        )
      }.fork{

        c.io.peOut(0).expectDequeue(
          chiselTypeOf(c.io.peOut(0)).bits.Lit(
            _.reward -> 69.U,
            _.last -> false.B
          )
        )
      }.join()
    }
  }


  val ap2 = new DataDistributorParams(
    nPEs = 4, bitWidth = 8, memWidth = 32,maxProblemSize = 8
  )

  it should "Pass a stream of data out correctly" in {
    test(new DataDistributorParUnO(ap2)) { c =>
      c.io.mem.initSource().setSourceClock(c.clock)
      c.io.peOut.map(_.initSink.setSinkClock(c.clock))

      fork {
        c.io.mem.enqueueSeq(Seq.tabulate(100)(idx =>
          chiselTypeOf(c.io.mem).bits.Lit(
            _.data -> ((idx << 24) | (idx << 16) | (idx << 8) | idx).U,
            _.mask -> 15.U,
            _.last -> false.B
          )))
      }. fork {
        c.io.peOut(0).expectDequeueSeq(Seq.tabulate(100)(idx =>
          chiselTypeOf(c.io.peOut(0)).bits.Lit(
            _.reward -> idx.U,
            _.last -> false.B
          )))
      }.fork {
      c.io.peOut(1).expectDequeueSeq(Seq.tabulate(100) (idx =>
        chiselTypeOf(c.io.peOut(0)).bits.Lit(
          _.reward -> idx.U,
          _.last -> false.B
        )))
    }.fork {
    c.io.peOut(2).expectDequeueSeq(Seq.tabulate(100) (idx =>
      chiselTypeOf(c.io.peOut(0)).bits.Lit(
        _.reward -> idx.U,
        _.last -> false.B
      )))
  }.fork {
        c.io.peOut(3).expectDequeueSeq(Seq.tabulate(100) (idx =>
          chiselTypeOf(c.io.peOut(0)).bits.Lit(
            _.reward -> idx.U,
            _.last -> false.B
          )))
      }
    .join()
}
}
}
