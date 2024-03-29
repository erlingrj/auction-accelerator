package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._


class TestAccountant extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation)

  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new AssignmentEngineParams(
    nPEs = 4, bitWidth = 8, mrp=mp, maxProblemSize = 16
  )

  def expectRead(c: AssignmentEngine, addr: Int, res: Int) = {
  fork {
    c.io.bramStoreReadAddr.expect(addr.U)
  } .fork {
    c.io.bramStoreReadData.poke(res.U)
  }.join()
  }

  def initClocks(c: AssignmentEngine): Unit = {
    c.io.searchResultIn.initSource().setSourceClock(c.clock)
    c.io.unassignedAgents.initSink().setSinkClock(c.clock)
    c.io.requestedAgents.initSource().setSourceClock(c.clock)
    c.io.writeBackStream.wrData.initSink().setSinkClock(c.clock)
  }

  def initRF(c: AssignmentEngine): Unit = {
    c.io.rfInfo.nObjects.poke(8.U)
    c.io.rfInfo.nAgents.poke(8.U)
  }

  def mockAssignment(c: AssignmentEngine, agent: Int, obj: Int, bid: Int, oldPrice: Int): Unit = {
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
    }.fork.withRegion(Monitor) {
      c.io.bramStoreReadAddr.expect(obj.U)
      c.io.bramStoreReadData.poke(oldPrice.U)
      c.clock.step()
      if (bid > oldPrice) {
        c.io.bramStoreWriteDataValid.expect(true.B)
        c.io.bramStoreWriteData.expect(bid.U)
        c.io.bramStoreWriteAddr.expect(obj.U)
        c.clock.step()
      }
    }.joinAndStep(c.clock)
  }

  behavior of "AccountantExtPricePipelined"

  it should "Initialize correctly" in {
    test(new AssignmentEngine(ap)) { c =>
      c.io.unassignedAgents.ready.poke(true.B)
      c.io.requestedAgents.valid.poke(true.B)

      c.io.unassignedAgents.valid.expect(false.B)
      c.io.searchResultIn.ready.expect(true.B)
      c.io.bramStoreWriteDataValid.expect(false.B)
      c.io.writeBackStream.start.expect(false.B)
    }
  }

  it should "update price correctly" in {
    test(new AssignmentEngine(ap)) { c =>
      initClocks(c)
      c.io.unassignedAgents.ready.poke(true.B)
      mockAssignment(c, agent=5, obj=6, bid=10, oldPrice=0)
      c.clock.step(5)
    }
  }

  it should "evict old agent and fire memory request" in {
    test(new AssignmentEngine(ap)) { c =>
      initClocks(c)
      c.io.rfInfo.nObjects.poke(8.U)
      fork {
        mockAssignment(c, agent=5, obj=6, bid=10, oldPrice=0)
        mockAssignment(c, agent=6, obj=6, bid=12, oldPrice=10)
      }.fork {
        c.io.unassignedAgents.expectDequeue(
          chiselTypeOf(c.io.unassignedAgents).bits.Lit(
            _.agent -> 5.U,
            _.nObjects -> 0.U
          )
        )
      }.join()
    }
  }

//  it should "writeback correct prices" in {
//
//    test(new AccountantExtPriceNonPipelined(ap)) { c =>
//      initClocks(c)
//      initRF(c)
//      c.io.unassignedAgents.ready.poke(true.B)
//      mockAssignment(c, agent=1, obj=0, bid=10, oldPrice=0)
//      mockAssignment(c, agent=0, obj=1, bid=12, oldPrice = 0)
//      mockAssignment(c, agent=3, obj=2, bid=7, oldPrice = 0)
//      mockAssignment(c, agent=2, obj=3, bid=8, oldPrice = 0)
//      mockAssignment(c, agent=6, obj=4, bid=2, oldPrice = 0)
//      mockAssignment(c, agent=5, obj=5, bid=33, oldPrice = 0)
//      mockAssignment(c, agent=7, obj=6, bid=1, oldPrice = 0)
//      mockAssignment(c, agent=4, obj=7, bid=10, oldPrice = 0)
//
//      c.clock.step(2)
//      c.io.doWriteBack.poke(true.B)
//
//      val prices = Seq(10, 12, 7,8,2,33,1,10)
//
//      fork {
//        c.io.writeBackStream.wrData.expectDequeueSeq(Seq(1.U,0.U,3.U,2.U,6.U,5.U,7.U,4.U))
//        c.io.writeBackStream.wrData.expectDequeueSeq(prices.map(_.U))
//      }.fork {
//        for (i <- prices.indices) {
//          expectRead(c, i, prices(i))
//        }
//      }.join()
//
//    }
//  }

  it should "not schedule new unassigned" in {
    test(new AssignmentEngine(ap)) { c =>
      initClocks(c)
      initRF(c)
      c.io.unassignedAgents.ready.poke(true.B)
      mockAssignment(c, agent=3, obj=2,bid=10, oldPrice=0)
      c.io.unassignedAgents.valid.expect(false.B)
      for (i <- 0 until 10) {
        c.clock.step(1)
        c.io.unassignedAgents.valid.expect(false.B)
      }
    }
  }

  it should "catch misspeculation 1" in {
    test(new AssignmentEngine(ap)) { c =>
      initClocks(c)
      c.io.rfInfo.nObjects.poke(8.U)
      fork {
        mockAssignment(c, agent = 5, obj = 6, bid = 12, oldPrice = 0)
        mockAssignment(c, agent = 6, obj = 6, bid = 10, oldPrice = 12)
      }.fork {
        c.io.unassignedAgents.expectDequeue(
          chiselTypeOf(c.io.unassignedAgents).bits.Lit(
            _.agent -> 6.U,
            _.nObjects -> 0.U
          )
        )
      }.join()
    }
  }

  it should "catch misspeculation 2" in {
    test(new AssignmentEngine(ap)) { c =>
      initClocks(c)
      c.io.rfInfo.nObjects.poke(8.U)
      fork {
        mockAssignment(c, agent = 5, obj = 6, bid = 12, oldPrice = 0)
        mockAssignment(c, agent = 6, obj = 6, bid = 12, oldPrice = 12)
      }.fork {
        c.io.unassignedAgents.expectDequeue(
          chiselTypeOf(c.io.unassignedAgents).bits.Lit(
            _.agent -> 6.U,
            _.nObjects -> 0.U
          )
        )
      }.join()
    }
  }
}
