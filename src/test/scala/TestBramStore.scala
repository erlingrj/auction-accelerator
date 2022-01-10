package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.legacy.backends.verilator.{VerilatorCFlags, VerilatorFlags}
import chisel3.experimental.BundleLiterals._

class TestBramStore extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq("-y /home/erling/dev/chisel/auction-accelerator/fpga-tidbits/src/main/resources/verilog")))
  val ap = new BramStoreParams(
    nPEs = 4, bitWidth = 8, maxProblemSize = 16
  )


  def writePrices(c: BramStore, addrs: Seq[Int], prices: Seq[Int]): Unit = {
    for ((a,v) <- (addrs zip prices) ) {
      writePrice(c,a,v)
    }
  }

  def expectReadPrice(c: BramStore, addr: Int, price: Int, idx: Int): Unit = {
    if (idx == ap.nPEs) {
      c.io.assReadReq.poke(addr.U)
      c.clock.step(1)
      c.io.assReadRsp.expect(price.U)
    } else {
      c.io.peReadReq(idx).poke(addr.U)
      c.clock.step()
      c.io.peReadRsp(idx).expect(price.U)
    }
  }

  def writePrice(c: BramStore, addr: Int, price: Int): Unit = {
    c.io.assWriteReq.enqueue(
      chiselTypeOf(c.io.assWriteReq).bits.Lit(
        _.addr -> addr.U,
        _.data -> price.U
      )
    )
  }

  def expectPrices(c: BramStore, p: Seq[Int], a: Seq[Int]): Unit = {
    for (i <- p.indices) {
      expectReadPrice(c, a(i), p(i), i)
    }
  }

  def initClocks(c: BramStore): Unit = {
    c.io.assWriteReq.initSource().setSourceClock(c.clock)
  }

  behavior of "BramStore"
  it should "Initialize correctly" in {
    test(new BramStore(ap)).withAnnotations(verilator) { c =>
      initClocks(c)
      c.io.assReadRsp.expect(0.U)
      expectPrices(c, Seq(0,0,0,0), Seq(0,0,0,0))
    }
  }

  it should "Write and read prices" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      expectReadPrice(c, 0, 0, 0)
      writePrice(c, 8, 128)
      c.clock.step(2)
      expectReadPrice(c, 8, 128, 0)
    }
  }

  it should "forward write->read" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrice(c, 15, 32)
      expectReadPrice(c, 15, 32, 0)
    }
  }


  it should "PE idx iface work" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrices(c, (0 until 16).toSeq, (0 until 16).toSeq)
      expectPrices(c, Seq(1,2,3,4), Seq(1,2,3,4))
    }
  }

  it should "PE idx work with stall" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrices(c, (0 until 16).toSeq, (0 until 16).toSeq)
      expectPrices(c, Seq(0,0,0,0), Seq(0,0,0,0))
      expectPrices(c, Seq(1,2,3,4), Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4), Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4), Seq(1,2,3,4))
      writePrice(c, 4,5)
      expectPrices(c,  Seq(1,2,3,5), Seq(1,2,3,4))
    }
  }
}

