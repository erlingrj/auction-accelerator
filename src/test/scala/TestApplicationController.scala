package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._


class TestApplicationController extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation)


  def initClocks(c: ApplicationController): Unit = {
    c.io.requestedAgentsOut.initSink().setSinkClock(c.clock)
    c.io.unassignedAgentsOut.initSink().setSinkClock(c.clock)
    c.io.requestedAgentsIn.initSource().setSourceClock(c.clock)
    c.io.unassignedAgentsIn.initSource().setSourceClock(c.clock)
  }

  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new ApplicationControllerParams(
    nPEs = 4, bitWidth = 8, mrp = mp, maxProblemSize = 100
  )
  behavior of "Controller"

  it should "Initialize correctly" in {
    test(new ApplicationController(ap)) { c =>
      c.io.unassignedAgentsOut.valid.expect(false.B)
      c.io.requestedAgentsOut.valid.expect(false.B)
      c.io.doWriteBack.expect(false.B)

      c.io.rfCtrl.finished.expect(false.B)
      c.io.rfCtrl.cycleCount.expect(0.U)
    }
  }

  it should "pass memoryRequested through" in {
    test(new ApplicationController(ap)) { c =>
      initClocks(c)
      fork {
        c.io.requestedAgentsIn.enqueueNow(
          chiselTypeOf(c.io.requestedAgentsIn).bits.Lit(
            _.agent -> 0.U,
            _.nObjects -> 8.U
          )
        )
      }.fork {
        c.io.requestedAgentsOut.expectDequeueNow(
          chiselTypeOf(c.io.requestedAgentsIn).bits.Lit(
            _.agent->0.U,
            _.nObjects-> 8.U
          )
        )
      }.join()
    }
  }

  it should "generate first set of mem reqs" in {
    test(new ApplicationController(ap)) { c =>
      initClocks(c)
      val nAgents = 90
      val nObjects = 70
      c.io.rfInfo.baseAddr.poke(64.U)
      c.io.rfInfo.nAgents.poke(nAgents.U)
      c.io.rfInfo.nObjects.poke(nObjects.U)
      c.io.rfInfo.start.poke(true.B)

      c.clock.step()
      c.io.dram2bram_start.expect(true.B)
      c.io.dram2bram_baseAddr.expect(64.U)
      c.io.dram2bram_nRows.expect(nAgents.U)
      c.io.dram2bram_nCols.expect(nObjects.U)

      c.clock.step(10)
      c.io.dram2bram_finished.poke(true.B)
      c.clock.step()

      c.io.unassignedAgentsOut.expectDequeueSeq(Seq.tabulate(nAgents)(idx =>
        chiselTypeOf(c.io.unassignedAgentsOut).bits.Lit(
          _.agent -> (nAgents-idx-1).U, //starts from the highest
          _.nObjects -> nObjects.U
        )
      ))
    }
  }
}
