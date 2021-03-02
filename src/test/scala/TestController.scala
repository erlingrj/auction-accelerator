package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._


class TestController extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation)

  object ap extends AuctionParams {
    val nPEs = 4
    val bitWidth = 8
    val memWidth = 64
    val maxProblemSize = 100
  }

  def initClocks(c: Controller): Unit = {
    c.io.memoryRequestedOut.initSink().setSinkClock(c.clock)
    c.io.memoryRequestOut.initSink().setSinkClock(c.clock)
    c.io.memoryRequestedIn.initSource().setSourceClock(c.clock)
    c.io.memoryRequestIn.initSource().setSourceClock(c.clock)
  }

  val mp = new MemReqParams(32, 64, 6, 1, true)
  behavior of "Controller"

  it should "Initialize correctly" in {
    test(new Controller(ap, mp)) { c =>
      c.io.memoryRequestOut.valid.expect(false.B)
      c.io.memoryRequestedOut.valid.expect(false.B)
      c.io.doWriteBack.expect(false.B)

      c.io.rfCtrl.finished.expect(false.B)
      c.io.rfCtrl.cycleCount.expect(0.U)
    }
  }

  it should "pass memoryRequested through" in {
    test(new Controller(ap, mp)) { c =>
      initClocks(c)
      fork {
        c.io.memoryRequestedIn.enqueueNow(
          chiselTypeOf(c.io.memoryRequestedIn).bits.Lit(
            _.agent -> 0.U,
            _.nObjects -> 8.U
          )
        )
      }.fork {
        c.io.memoryRequestedOut.expectDequeueNow(
          chiselTypeOf(c.io.memoryRequestedIn).bits.Lit(
            _.agent->0.U,
            _.nObjects-> 8.U
          )
        )
      }.join()
    }
  }

  it should "generate first set of mem reqs" in {
    test(new Controller(ap, mp)) { c =>
      initClocks(c)
      val nAgents = 90
      val nObjects = 70
      c.io.rfInfo.baseAddr.poke(64.U)
      c.io.rfInfo.nAgents.poke(nAgents.U)
      c.io.rfInfo.nObjects.poke(nObjects.U)
      c.io.rfInfo.start.poke(true.B)

      c.io.memoryRequestOut.expectDequeueSeq(Seq.tabulate(nAgents)( idx =>
        chiselTypeOf(c.io.memoryRequestOut).bits.Lit(
          _.agent -> (nAgents-idx).U, //starts from the highest
          _.nObjects -> nObjects.U
        )
      ))
    }
  }
}
