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


  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new MemCtrlParams(
    nPEs = 4, bitWidth = 8, mrp = mp, maxProblemSize = 64
  )

  def initClocks(c: MemoryController): Unit = {
    c.ioMem.req.initSink().setSinkClock(c.clock)
    c.ioMem.rsp.initSource().setSourceClock(c.clock)
    c.ioCtrl.unassignedAgents.initSource().setSourceClock(c.clock)
    c.ioCtrl.memData.initSink().setSinkClock(c.clock)
  }

  behavior of "MemoryController"

  it should "Initialize correctly" in {
    test(new AuctionDRAMController(ap)) { c =>
      c.ioMem.rsp.ready.expect(true.B)
      c.ioMem.req.valid.expect(false.B)
      c.ioCtrl.memData.valid.expect(false.B)
      c.ioCtrl.unassignedAgents.ready.expect(true.B)
    }
  }

  it should "generate simple memory request" in  {
    test(new AuctionDRAMController(ap)) { c =>
      initClocks(c)

      c.ioCtrl.requestedAgents.ready.poke(true.B)
      // Request 8x8bit words starting from address 0
      c.ioCtrl.regFile.baseAddr.poke(64.U)

      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(_.agent->0.U,_.nObjects -> 8.U))

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

    test(new AuctionDRAMController(ap)) { c =>
      initClocks(c)

      c.ioCtrl.requestedAgents.ready.poke(true.B)

      c.ioCtrl.regFile.baseAddr.poke(64.U)
      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueue(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.agent->0.U,
        _.nObjects -> 12.U
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

    test(new AuctionDRAMController(ap)).withAnnotations(verilator) { c =>
      initClocks(c)

      c.ioCtrl.requestedAgents.ready.poke(true.B)
      c.ioCtrl.regFile.baseAddr.poke(64.U)
      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.agent->64.U,
        _.nObjects -> 8.U
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

    test(new AuctionDRAMController(ap)).withAnnotations(verilator) { c =>
      initClocks(c)

      c.ioCtrl.requestedAgents.ready.poke(true.B)
      c.ioCtrl.regFile.baseAddr.poke(64.U)
      // Request 8x8bit words starting from address 0
      c.ioCtrl.unassignedAgents.enqueueNow(chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
        _.agent->64.U,
        _.nObjects -> 12.U
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


  it should "generate unaligned mem access" in {
    val ap = new MemCtrlParams(
      nPEs = 4, bitWidth = 16, mrp = mp, maxProblemSize = 64
    )

    test(new AuctionDRAMController(ap)) { c =>
      initClocks(c)

      // Request 8x8bit words starting from address 0
      c.ioCtrl.regFile.baseAddr.poke(0.U)
      c.ioCtrl.requestedAgents.ready.poke(true.B)
      c.ioCtrl.unassignedAgents.enqueueNow(
        chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
          _.agent -> 1.U,
          _.nObjects -> 5.U
        ))

      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr -> 16.U,
        _.numBytes -> 8.U,
        _.isWrite -> false.B,
        _.channelID -> 0.U,
        _.metaData -> 0.U
      ))
      c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
        _.addr -> 24.U,
        _.numBytes -> 8.U,
        _.isWrite -> false.B,
        _.channelID -> 0.U,
        _.metaData -> 0.U
      ))

    }
  }

    it should "generate unaligned mem access2" in {
      val ap = new MemCtrlParams(
        nPEs = 4, bitWidth = 16, mrp=mp, maxProblemSize = 64
      )

      test(new AuctionDRAMController(ap)) { c =>
        initClocks(c)

        // Request 8x8bit words starting from address 0
        c.ioCtrl.regFile.baseAddr.poke(0.U)
        c.ioCtrl.requestedAgents.ready.poke(true.B)

        c.ioCtrl.unassignedAgents.enqueueNow(
          chiselTypeOf(c.ioCtrl.unassignedAgents).bits.Lit(
            _.agent -> 3.U,
            _.nObjects -> 5.U
          ))

        c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
          _.addr -> 48.U,
          _.numBytes -> 8.U,
          _.isWrite -> false.B,
          _.channelID -> 0.U,
          _.metaData -> 0.U
        ))
        c.ioMem.req.expectDequeue(chiselTypeOf(c.ioMem.req).bits.Lit(
          _.addr -> 56.U,
          _.numBytes -> 8.U,
          _.isWrite -> false.B,
          _.channelID -> 0.U,
          _.metaData -> 0.U
        ))
      }
    }
}
