package auction


import chisel3.experimental.BundleLiterals._
import org.scalatest._
import chiseltest._
import chisel3._

class TestPricesNoFuzz extends FlatSpec with ChiselScalatestTester with Matchers {


  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
    val memWidth = 32
    val maxProblemSize = 8
  }

  def initClocks(c: PricesNoFuzz): Unit = {
    c.io.peResps.map(_.initSink().setSinkClock(c.clock))
    c.io.peReqs.map(_.initSource().setSourceClock(c.clock))
    c.io.priceUpdate.initSource().setSourceClock(c.clock)
  }
  behavior of "PricesNoFuzz"
  it should "Initialize correctly" in {
    test(new PricesNoFuzz(AuctionTestParams)) { c =>
      c.io.peReqs.map(_.ready.expect(true.B))
      c.io.priceUpdate.ready.expect(false.B)
      c.io.peResps.map(_.valid.expect(false.B))
    }
  }

  it should "Give prices and block" in {
    test(new PricesNoFuzz(AuctionTestParams)) { c =>
      initClocks(c)

      fork {
        c.io.last.poke(true.B)
        c.io.peReqs(0).enqueue(chiselTypeOf(c.io.peReqs(0)).bits.Lit(
          _.objectId->3.U
        ))
      }.fork {
        c.io.peResps(0).expectDequeue(chiselTypeOf(c.io.peResps(0)).bits.Lit(
          _.price->0.U
        ))
      }.joinAndStep(c.clock)

      c.io.peReqs.map(_.ready.expect(false.B))
      c.io.peResps.map(_.valid.expect(false.B))

    }
  }
}


