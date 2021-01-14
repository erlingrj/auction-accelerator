package auction

import org.scalatest._
import chiseltest._
import chisel3._
import fpgatidbits.PlatformWrapper._

import fpgatidbits.PlatformWrapper.GenericAccelImplicits._

class TestAuction extends FlatSpec with ChiselScalatestTester with Matchers {

  object Ap extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
  }

  behavior of "AuctionAlgo"

  it should "work on simple 4x4 example" in {
    test(new TesterWrapper({ p => new Auction(p) }, "_dump")) { c =>
      c.writeReg("nRows", 4.U)
      c.writeReg("nCols", 4.U)
      c.writeReg("baseAddr", 0.U)

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0, rewardArr)
      c.writeReg("start", 1.U)

      var cnt = 0
      while (c.readReg("finished").litValue != 1 &&
        cnt < 100) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("finished", 1.U)
    }
  }


  it should "read/write multiple matrix to memory" in {
    test(new TesterWrapper({ p => new Auction(p) }, "_dump")) { c =>

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0, rewardArr)
      var addr = 0
      for (v <- rewardArr) {
        c.expectMem(addr, v)
        addr = addr + 8
      }
    }
  }

  it should "Read/Write CSR" in {
    test(new TesterWrapper({ p => new Auction(p) }, "_dump")) { c =>

      c.writeReg("nRows", 4.U)
      c.expectReg("nRows", 4.U)
    }
  }
  it should "read/write to mem" in {
    test(new TesterWrapper({ p => new Auction(p) }, "_dump")) { c =>
      c.writeMem(8, 69.U)
      c.expectMem(8, 69.U)
    }
  }
}