package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._
import auction.TestDRAM2BRAM



class TestBramController extends FlatSpec with ChiselScalatestTester with Matchers {

  val t = new TestDRAM2BRAM

  val verilator = Seq(VerilatorBackendAnnotation)


  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new MemCtrlParams(
    nPEs = 4, bitWidth = 8, mrp = mp, maxProblemSize = 64
  )

  def initClocks(c: BramController): Unit = {
    c.io.unassignedAgents.initSource().setSourceClock(c.clock)
    c.io.dataDistOut.initSink().setSinkClock(c.clock)
    c.io.requestedAgents.initSink().setSinkClock(c.clock)
    c.io.agentRowAddrReq.req.initSink().setSinkClock(c.clock)
    c.io.agentRowAddrReq.rsp.initSource().setSourceClock(c.clock)

  }

  def getAgentRowInfo(c: BramController, addr: Int, row: Int, length: Int) = {
    fork{
    c.io.agentRowAddrReq.req.expectDequeue(chiselTypeOf(c.io.agentRowAddrReq.req).bits.Lit(
      _.addr -> addr.U,
      _.wen->false.B,
      _.wdata.rowAddr -> 0.U,
      _.wdata.length -> 0.U
    ))
    }.fork {
    c.io.agentRowAddrReq.rsp.enqueue(chiselTypeOf(c.io.agentRowAddrReq.rsp).bits.Lit(
      _.rdata.rowAddr -> row.U,
      _.rdata.length -> length.U
    ))
    }.join()
  }

  def getBramRow(c: BramController, row: Int, data: BigInt) = {
    c.clock.step(1)
    c.io.bramReq.rsp.readData.poke(data.U)
    c.clock.step()
  }

  def tuple2BramEl(c: BramController, t: (Int,Int)): BigInt = {
    val res = BigInt((t._1)) | BigInt(t._2 << (c.p.agentWidth))
    res
  }

  def bramEls2BramLine(c: BramController, e: Seq[BigInt]): BigInt = {
    var res: BigInt = 0
    e.zipWithIndex.foreach((v) => {
      res = res | v._1 << (v._2 * (c.p.bitWidth + c.p.agentWidth));
    }
    )
    res
  }

  def expectDataDistNow(c: BramController, bramEls: Seq[(Int,Int)], last: Boolean) = {
    c.io.dataDistOut.ready.poke(true.B)
    c.io.dataDistOut.valid.expect(true.B)
    c.io.dataDistOut.bits.last.expect(last.B)
      c.io.dataDistOut.bits.valids(0).expect((bramEls(0)._2 > 0).B)
      c.io.dataDistOut.bits.valids(1).expect((bramEls(1)._2 > 0).B)
      c.io.dataDistOut.bits.valids(2).expect((bramEls(2)._2 > 0).B)
      c.io.dataDistOut.bits.valids(3).expect((bramEls(3)._2 > 0).B)
      c.io.dataDistOut.bits.els(0).reward.expect(bramEls(0)._2.U)
      c.io.dataDistOut.bits.els(0).idx.expect(bramEls(0)._1.U)
      c.io.dataDistOut.bits.els(1).reward.expect(bramEls(1)._2.U)
      c.io.dataDistOut.bits.els(1).idx.expect(bramEls(1)._1.U)
      c.io.dataDistOut.bits.els(2).reward.expect(bramEls(2)._2.U)
      c.io.dataDistOut.bits.els(2).idx.expect(bramEls(2)._1.U)
      c.io.dataDistOut.bits.els(3).reward.expect(bramEls(3)._2.U)
      c.io.dataDistOut.bits.els(3).idx.expect(bramEls(3)._1.U)
  }



  behavior of "BramController"

it should "Initialize correctly" in {
    test(new BramController(ap)) { c =>
      c.io.unassignedAgents.ready.expect(true.B)
      c.io.dataDistOut.valid.expect(false.B)
      c.io.requestedAgents.valid.expect(false.B)

    }
  }

  it should "Request row from RegStore" in {
    test(new BramController(ap)) { c =>
      initClocks(c)
        fork {
          c.io.unassignedAgents.enqueue(chiselTypeOf(c.io.unassignedAgents).bits.Lit(
            _.agent -> 8.U,
            _.nObjects -> 0.U
          ))
        }.fork {
          getAgentRowInfo(c, 8, 3,1)
        }.join()
    }
  }
  it should "Request data from BRAM" in {
    test(new BramController(ap)) { c =>
      initClocks(c)
      val bramLine = bramEls2BramLine(c, Seq((12, 0), (14,2), (15,4), (2,8)).map(tuple2BramEl(c,_)))
      println(bramLine)
      fork {
        c.io.unassignedAgents.enqueue(chiselTypeOf(c.io.unassignedAgents).bits.Lit(
          _.agent -> 8.U,
          _.nObjects -> 0.U
        ))
      }.fork {
        getAgentRowInfo(c, 8, 3,1)
      }.fork {
        getBramRow(c, 3, bramLine)
      }.join()
    }
  }

  it should "forward correct data to datadist" in {
    test(new BramController(ap)) { c =>
      initClocks(c)
      val bramEls = Seq((0, 0x10), (2, 0x11), (4, 0x12), (8, 0x13))
      val bramLine = bramEls2BramLine(c, bramEls.map(tuple2BramEl(c, _)))
      println(f"$bramLine%X")
      println(f"$bramLine")
      fork {
        c.io.unassignedAgents.enqueue(chiselTypeOf(c.io.unassignedAgents).bits.Lit(
          _.agent -> 8.U,
          _.nObjects -> 0.U
        ))
      }.fork {
        getAgentRowInfo(c, 8, 3, 1)
      }.fork {
        getBramRow(c, 3, bramLine)
      }.join()

      c.clock.step()

        c.io.requestedAgents.ready.poke(true.B)
        expectDataDistNow(c, bramEls, true)
    }
  }

  it should "forward correct data to datadist over multiple rounds" in {
    test(new BramController(ap)) { c =>
      initClocks(c)
      val bramEls1 = Seq((0, 0x10), (1, 0x11), (2, 0x12), (3, 0x13))
      val bramEls2 = Seq((4, 4), (5,5), (6,6), (7,7))
      val bramLine1 = bramEls2BramLine(c, bramEls1.map(tuple2BramEl(c, _)))
      val bramLine2 = bramEls2BramLine(c, bramEls2.map(tuple2BramEl(c, _)))
      fork {
        c.io.unassignedAgents.enqueue(chiselTypeOf(c.io.unassignedAgents).bits.Lit(
          _.agent -> 8.U,
          _.nObjects -> 0.U
        ))
      }.fork {
        getAgentRowInfo(c, 8, 8, 2)
      }.fork {
        getBramRow(c, 8, bramLine1)
      }.join()
      c.clock.step(1)
      c.io.requestedAgents.ready.poke(true.B)
      expectDataDistNow(c, bramEls1, false)
      getBramRow(c, 9, bramLine2)
      expectDataDistNow(c, bramEls2, true)
    }

  }
}