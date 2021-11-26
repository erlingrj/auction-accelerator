package auction


import chisel3.experimental.BundleLiterals._
import org.scalatest._
import chiseltest._
import chisel3._

class TestProcessingElement extends FlatSpec with ChiselScalatestTester with Matchers {

  val ap = new ProcessingElementParams(
    nPEs = 4, bitWidth = 32, maxProblemSize = 16
  )

  def initClocks(c: ProcessingElement): Unit = {
    c.io.rewardIn.initSource.setSourceClock(c.clock)
    c.io.PEResultOut.initSink.setSinkClock(c.clock)
  }

  def expectPriceReq(c: ProcessingElement, addr: Int): Unit = {
    c.io.agentIdx.expect(addr.U)
    c.io.agentIdxReqValid.expect(true.B)
  }

  def enqPriceRsp(c: ProcessingElement, data: Int): Unit = {
    c.io.price.poke(data.U)
  }

  behavior of "ProcessingElementExtPrice"
  it should "Initialize correctly" in {
    test(new ProcessingElement(ap)) { c =>
      c.io.PEResultOut.ready.poke(true.B)
      c.io.PEResultOut.valid.expect(false.B)
      c.io.agentIdxReqValid.expect(false.B)
      c.io.rewardIn.ready.expect(true.B)
    }
  }
  it should "Do right calc" in {
    test(new ProcessingElement(ap)) { c =>
      initClocks(c)
        fork {
          c.io.price.poke(98.U)
          c.io.rewardIn.enqueue(chiselTypeOf(c.io.rewardIn).bits.Lit(
            _.last -> false.B,
            _.reward -> 100.U,
            _.idx -> 8.U))
        }.fork.withRegion(Monitor) {
          expectPriceReq(c,8)
        }.fork {
          c.io.PEResultOut.expectDequeue(chiselTypeOf(c.io.PEResultOut).bits.Lit(
            _.benefit -> 2.U,
            _.id -> 8.U,
            _.last -> false.B,
            _.oldPrice -> 98.U
          ))
        }.joinAndStep(c.clock)
    }
  }

}

