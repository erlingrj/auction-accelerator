package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import fpgatidbits.dma._


class TestMemoryController extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation)

  object ap extends AuctionParams {
    val nPEs = 4
    val bitWidth = 8
    val memWidth = 64
    val maxProblemSize = 64
  }

  def initClocks(c: MemoryController): Unit = {
    c.ioMem.req.initSink().setSinkClock(c.clock)
    c.ioMem.rsp.initSource().setSourceClock(c.clock)
    c.ioCtrl.unassignedAgents.initSource().setSourceClock(c.clock)
    c.ioCtrl.memData.initSink().setSinkClock(c.clock)
  }

  val mp = new MemReqParams(32, 64, 6, 1, true)

  behavior of "MemoryController"

  it should "Initialize correctly" in {
    test(new AuctionDRAMController(ap, mp)) { c =>
      c.ioMem.rsp.ready.expect(false.B)
      c.ioMem.req.valid.expect(false.B)
      c.ioCtrl.memData.valid.expect(false.B)
      c.ioCtrl.unassignedAgents.ready.expect(true.B)
    }
  }

  it should "generate simple memory request" in  {
    test(new AuctionDRAMController(ap, mp)) { c =>
      initClocks(c)

      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(_.baseAddr->64.U,_.numWords -> 8.U))

      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr->64.U,
        _.numBytes->8.U,
        _.isWrite->false.B,
        _.channelID->0.U,
        _.metaData->0.U
      ))
    }
  }

  it should "generate complex memory request" in {

    test(new AuctionDRAMController(ap, mp)) { c =>
      initClocks(c)

      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueue(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.baseAddr->64.U,
        _.numWords -> 12.U
      ))

      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr->64.U,
        _.numBytes->8.U,
        _.isWrite->false.B,
        _.channelID->0.U,
        _.metaData->0.U
      ))

      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr->(64+8).U,
        _.numBytes->8.U,
        _.isWrite->false.B,
        _.channelID->0.U,
        _.metaData->0.U
      ))
    }
  }

  it should "handle simple memory response" in {

    test(new AuctionDRAMController(ap, mp)).withAnnotations(verilator) { c =>
      initClocks(c)

      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.baseAddr->64.U,
        _.numWords -> 8.U
      ))

      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr->64.U,
        _.numBytes->8.U,
        _.isWrite->false.B,
        _.channelID->0.U,
        _.metaData->0.U
      ))

      c.ioCtrl.memData.ready.poke(true.B)

      c.clock.step(2)

      fork {

        c.ioMem.rsp.enqueue(chiselTypeOf(c.ioMem.rsp).bits.Lit(
          _.channelID->0.U,
          _.isWrite->false.B,
          _.isLast->true.B,
          _.metaData->0.U,
          _.readData->"h0102_0304_0506_0708".U
        ))
      }.
        fork {
        c.ioCtrl.memData.expectDequeue(chiselTypeOf(c.ioCtrl.memData).bits.Lit(
          _.data->"h0102_0304_0506_0708".U,
          _.mask ->"hff".U,
          _.last -> true.B
        ))
      }.join()
      c.clock.step(10)
  }
  }
  it should "handle comples memory response" in {

    test(new AuctionDRAMController(ap, mp)).withAnnotations(verilator) { c =>
      initClocks(c)

      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.baseAddr->64.U,
        _.numWords -> 12.U
      ))

      c.ioMem.req.expectDequeueSeq(Seq(
        (chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr->64.U,
        _.numBytes->8.U,
        _.isWrite->false.B,
        _.channelID->0.U,
        _.metaData->0.U
      )),
        (chiselTypeOf(c.ioMem.req).bits.Lit(
          _.addr->72.U,
          _.numBytes->8.U,
          _.isWrite->false.B,
          _.channelID->0.U,
          _.metaData->0.U
        ))
      ))



      c.ioCtrl.memData.ready.poke(true.B)

      c.clock.step(2)

      fork {

        c.ioMem.rsp.enqueueSeq(Seq(
          chiselTypeOf(c.ioMem.rsp).bits.Lit(
          _.channelID->0.U,
          _.isWrite->false.B,
          _.isLast->true.B,
          _.metaData->0.U,
          _.readData->"h0102_0304_0506_0708".U
        ),
          chiselTypeOf(c.ioMem.rsp).bits.Lit(
            _.channelID->0.U,
            _.isWrite->false.B,
            _.isLast->true.B,
            _.metaData->0.U,
            _.readData->"h0910_1112_1314_1516".U
          ),
        ))
      }.
        fork {
          c.ioCtrl.memData.expectDequeueSeq(Seq(
            chiselTypeOf(c.ioCtrl.memData).bits.Lit(
            _.data->"h0102_0304_0506_0708".U,
            _.mask ->"hff".U,
              _.last -> false.B
          ),
            chiselTypeOf(c.ioCtrl.memData).bits.Lit(
            _.data->"h0910_1112_1314_1516".U,
            _.mask ->"h0f".U,
              _.last -> true.B
            )))
        }.join()
      c.clock.step(10)
    }
  }
}
