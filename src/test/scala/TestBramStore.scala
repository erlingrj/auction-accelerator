package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.legacy.backends.verilator.{VerilatorCFlags, VerilatorFlags}

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

  def expectReadPrice(c: BramStore, addr: Int, price: Int): Unit = {
    c.io.accReadAddr.poke(addr.U)
    c.io.accReadData.expect(price.U)
  }


  def writePrice(c: BramStore, addr: Int, price: Int): Unit = {
    c.io.accWriteData.poke(price.U)
    c.io.accWriteAddr.poke(addr.U)
    c.io.accWriteDataValid.poke(true.B)
    c.clock.step()
    c.io.accWriteDataValid.poke(false.B)
  }

  def expectPrices(c: BramStore, p: Seq[Int]): Unit = {

    p.zipWithIndex.foreach{case (v,i) => c.io.prices(i).expect(v.U)}
  }

  def initClocks(c: BramStore): Unit = {
    c.io.idxs.initSource().setSourceClock(c.clock)
  }

  def enqIdxs(c: BramStore, idxs: Seq[Int]): Unit = {
    c.io.idxs.valid.poke(true.B)
    c.io.idxs.bits zip idxs map {
      case (l,r) => l.poke(r.U)
    }
    c.clock.step()
    c.io.idxs.valid.poke(false.B)
  }

  behavior of "BramStore"
  it should "Initialize correctly" in {
    test(new BramStore(ap)).withAnnotations(verilator) { c =>
      initClocks(c)
      c.io.accReadData.expect(0.U)
      expectPrices(c, Seq(0,0,0,0))
    }
  }

  it should "Write and read prices" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      expectReadPrice(c, 0, 0)
      writePrice(c, 8, 128)
      c.clock.step()
      expectReadPrice(c, 8, 128)
    }
  }

  it should "forward write->read" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrice(c, 15, 32)
      expectReadPrice(c, 15, 32)
    }
  }


  it should "PE idx iface work" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrices(c, (0 until 16).toSeq, (0 until 16).toSeq)
      expectPrices(c, Seq(0,0,0,0))
      enqIdxs(c, Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4))
    }
  }

  it should "PE idx work with stall" in {
    test(new BramStore(ap)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      initClocks(c)
      writePrices(c, (0 until 16).toSeq, (0 until 16).toSeq)
      expectPrices(c, Seq(0,0,0,0))
      enqIdxs(c, Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4))
      expectPrices(c, Seq(1,2,3,4))
      writePrice(c, 4,5)
      expectPrices(c, Seq(1,2,3,5))
    }
  }
}

