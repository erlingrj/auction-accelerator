package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._


class TestAccountantNonPipelined extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation)

  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new AccountantParams(
    nPEs = 4, bitWidth = 8, mrp=mp, maxProblemSize = 16
  )

  def initClocks(c: Accountant): Unit = {
    c.io.searchResultIn.initSource().setSourceClock(c.clock)
    c.io.unassignedAgents.initSink().setSinkClock(c.clock)
    c.io.requestedAgents.initSource().setSourceClock(c.clock)
    c.io.PEControlOut.map(_.initSink().setSinkClock(c.clock))
    c.io.writeBackStream.wrData.initSink().setSinkClock(c.clock)
  }

  def initRF(c: Accountant): Unit = {
    c.io.rfInfo.nObjects.poke(8.U)
    c.io.rfInfo.nAgents.poke(8.U)
  }

  def mockAssignment(c: Accountant, agent: Int, obj: Int, bid: Int): Unit = {
      fork {
        c.io.requestedAgents.enqueue(
          chiselTypeOf(c.io.requestedAgents).bits.Lit(
            _.agent -> agent.U,
            _.nObjects -> 8.U
          ))
      }.fork {
        c.io.searchResultIn.enqueue(
          chiselTypeOf(c.io.searchResultIn).bits.Lit(
            _.bid -> bid.U,
            _.winner -> obj.U
          )
        )
      }.join()
  }

  behavior of "AccountantNonPipelined"

  it should "Initialize correctly" in {
    test(new AccountantNonPipelined(ap)) { c =>
      c.io.unassignedAgents.valid.expect(false.B)
      c.io.PEControlOut.map(_.bits.prices.map(_.expect(0.U)))
    }
  }

  it should "update price correctly" in {
    test(new AccountantNonPipelined(ap)) { c =>
      initClocks(c)
      c.io.unassignedAgents.ready.poke(true.B)
      mockAssignment(c, agent=5, obj=6, bid=10)
        c.clock.step(5)
        c.io.PEControlOut(2).bits.prices(1).expect(10.U)
    }
  }

  it should "evict old agent and fire memory request" in {
    test(new AccountantNonPipelined(ap)) { c =>
      initClocks(c)
      c.io.rfInfo.nObjects.poke(8.U)
      fork {
        mockAssignment(c, agent=5, obj=6, bid=10)
        mockAssignment(c, agent=6, obj=6, bid=12)
      }.fork {
        c.io.unassignedAgents.expectDequeue(
          chiselTypeOf(c.io.unassignedAgents).bits.Lit(
            _.agent -> 5.U,
            _.nObjects -> 8.U
          )
        )
      }.join()
    }
  }

  it should "writeback correct prices" in {

    test(new AccountantNonPipelined(ap)) { c =>
      initClocks(c)
      initRF(c)
      c.io.unassignedAgents.ready.poke(true.B)
      mockAssignment(c, agent=1, obj=0, bid=10)
      mockAssignment(c, agent=0, obj=1, bid=12)
      mockAssignment(c, agent=3, obj=2, bid=7)
      mockAssignment(c, agent=2, obj=3, bid=8)
      mockAssignment(c, agent=6, obj=4, bid=2)
      mockAssignment(c, agent=5, obj=5, bid=33)
      mockAssignment(c, agent=7, obj=6, bid=1)
      mockAssignment(c, agent=4, obj=7, bid=10)

      c.clock.step(2)
      c.io.doWriteBack.poke(true.B)
      c.io.writeBackStream.wrData.expectDequeueSeq(Seq(1.U,0.U,3.U,2.U,6.U,5.U,7.U,4.U))
      c.io.writeBackStream.wrData.expectDequeueSeq(Seq(10.U, 12.U, 7.U,8.U,2.U,33.U,1.U,10.U))
    }
  }

  it should "not schedule new unassigned" in {
    test(new AccountantNonPipelined(ap)) { c =>
      initClocks(c)
      initRF(c)
      c.io.unassignedAgents.ready.poke(true.B)
      mockAssignment(c, agent=3, obj=2,bid=10)
      c.io.unassignedAgents.valid.expect(false.B)
      for (i <- 0 until 10) {
        c.clock.step(1)
        c.io.unassignedAgents.valid.expect(false.B)
      }
    }
  }

}
