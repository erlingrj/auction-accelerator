package auction


import chisel3.experimental.BundleLiterals._
import org.scalatest._
import chiseltest._
import chisel3._

class TestProcessingElementExtPrice extends FlatSpec with ChiselScalatestTester with Matchers {

  val ap = new ProcessingElementParams(
    nPEs = 4, bitWidth = 32, maxProblemSize = 16
  )

  def initClocks(c: ProcessingElementExtPrice): Unit = {
    c.io.rewardIn.initSource.setSourceClock(c.clock)
    c.io.PEResultOut.initSink.setSinkClock(c.clock)
    c.io.priceStore.req.initSink.setSinkClock(c.clock)
    c.io.priceStore.rsp.initSource.setSourceClock(c.clock)
  }


  behavior of "ProcessingElementExtPrice"
  it should "Initialize correctly" in {
    test(new ProcessingElementExtPrice(ap)) { c =>

      c.io.PEResultOut.valid.expect(false.B)
      c.io.priceStore.rsp.valid.expect(false.B)
      c.io.rewardIn.ready.expect(true.B)
    }
  }
  it should "Do right calc" in {
    test(new ProcessingElementExtPrice(ap)) { c =>
      initClocks(c)

        fork {
          c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(
            _.last -> false.B,
            _.reward -> 100.U,
            _.idx -> 8.U))
        }.fork {
          c.io.priceStore.req.expectDequeue(chiselTypeOf(c.io.priceStore.req).bits.Lit(
            _.wen -> false.B,
            _.wdata -> 0.U,
            _.addr -> 8.U
          ))
        }.fork {
          c.io.priceStore.rsp.enqueue(chiselTypeOf(c.io.priceStore.rsp).bits.Lit(
            _.rdata -> 98.U
          ))
        }.join()

      c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(
        _.benefit -> 2.U,
        _.id -> 8.U,
        _.last -> false.B
      ))
    }
  }


}

