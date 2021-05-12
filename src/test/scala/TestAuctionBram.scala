package auction

import org.scalatest._
import chiseltest._
import chisel3._
import fpgatidbits.PlatformWrapper._

import fpgatidbits.PlatformWrapper.GenericAccelImplicits._

class TestAuctionBram extends FlatSpec with ChiselScalatestTester with Matchers {

  val ap = new AuctionParams(
    nPEs = 4, bitWidth = 16, memWidth = 64, maxProblemSize = 8
  )

  val rewMatBig: Seq[Seq[Long]] = Seq(
    Seq(7, 51, 52, 87, 38, 60, 74, 66, 0, 20),
    Seq(50, 12, 0, 64, 8, 53, 0, 46, 76, 42),
    Seq(27, 77, 0, 18, 22, 48, 44, 13, 0, 57),
    Seq(62, 0, 3, 8, 5, 6, 14, 0, 26, 39),
    Seq(0, 97, 0, 5, 13, 0, 41, 31, 62, 48),
    Seq(79, 68, 0, 0, 15, 12, 17, 47, 35, 43),
    Seq(76, 99, 48, 27, 34, 0, 0, 0, 28, 0),
    Seq(0, 20, 9, 27, 46, 15, 84, 19, 3, 24),
    Seq(56, 10, 45, 39, 0, 93, 67, 79, 19, 38),
    Seq(27, 0, 39, 53, 46, 24, 69, 46, 23, 1)
  )


  // This function creates a Sequence of 64 bit rows of UInts
  def generateMemoryArray(rows: Seq[Seq[Long]], width: Int): Seq[UInt] = {
    val nRows = rows.size
    val nCols = rows(0).size
    val valsPerMemRow = 64 / width
    var rewardArr = scala.collection.mutable.ArrayBuffer.empty[UInt]

    rows.foreach { case (row) =>
      row.sliding(valsPerMemRow, valsPerMemRow).foreach { case (w) =>
        var memWord: Long = 0
        w.zipWithIndex.foreach { case (v, i) =>
          memWord = memWord | v << i * width
        }
        rewardArr += memWord.U
      }
    }
    rewardArr
  }

  behavior of "AuctionBram"

  it should "10x10 with 8PEs" in {
    val ap = new AuctionParams(
      nPEs = 8, bitWidth = 8, memWidth = 64, maxProblemSize = 16
    )

    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      val nAgents = 10
      val nObjects = 10
      val baseAddr = 0
      val baseAddrRes = 1024
      c.writeReg("rfIn_nAgents", nAgents.U)
      c.writeReg("rfIn_nObjects", nObjects.U)
      c.writeReg("rfIn_baseAddr", baseAddr.U)
      c.writeReg("rfIn_baseAddrRes", baseAddrRes.U)

      rewMatBig.map{ row =>
        row.map{v =>
          print(f"$v%x ")
        }
        println("")
      }

      val rewardArr = generateMemoryArray(rewMatBig, 8)
      println(rewardArr)
      c.arrayToMem(baseAddr, rewardArr)
      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      var cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)

      val assignments = Seq(3, 4, 6, 0, 9, 8, 7, 5, 1, 2)

      for (j <- 0 until nObjects) {
        c.expectMem(baseAddrRes + 8 * j, assignments(j).U)
      }
      // TODO: check prices
    }
  }
  it should "TesterWrapper mem iface" in {
    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      c.writeReg("rfIn_nAgents", 4.U)
      c.writeReg("rfIn_nObjects", 4.U)
      c.writeReg("rfIn_baseAddr", 0.U)

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0, rewardArr)

      c.expectMem(0, "h0004_0003_0002_0001".U)
      c.expectMem(8, "h0003_0002_0001_0004".U)
      c.expectMem(16, "h0002_0001_0004_0003".U)
      c.expectMem(24, "h0001_0004_0003_0002".U)
    }
  }

  it should "work on multiple simple 4x4 example" in {
    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      c.writeReg("rfIn_nAgents", 4.U)
      c.writeReg("rfIn_nObjects", 4.U)
      c.writeReg("rfIn_baseAddr", 0.U)
      c.writeReg("rfIn_baseAddrRes", 32.U)

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0, rewardArr)
      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      var cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)
      c.expectMem(addr = 32, value = 1.U)
      c.expectMem(addr = 40, value = 2.U)
      c.expectMem(addr = 48, value = 3.U)
      c.expectMem(addr = 56, value = 0.U)


      c.expectMem(addr = 64, value = 1.U)
      c.expectMem(addr = 72, value = 1.U)
      c.expectMem(addr = 80, value = 1.U)
      c.expectMem(addr = 88, value = 1.U)


      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)
      c.expectMem(addr = 32, value = 1.U)
      c.expectMem(addr = 40, value = 2.U)
      c.expectMem(addr = 48, value = 3.U)
      c.expectMem(addr = 56, value = 0.U)


      c.expectMem(addr = 64, value = 1.U)
      c.expectMem(addr = 72, value = 1.U)
      c.expectMem(addr = 80, value = 1.U)
      c.expectMem(addr = 88, value = 1.U)

    }
  }
  it should "work on simple 4x4 example" in {
    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      c.writeReg("rfIn_nAgents", 4.U)
      c.writeReg("rfIn_nObjects", 4.U)
      c.writeReg("rfIn_baseAddr", 0.U)
      c.writeReg("rfIn_baseAddrRes", 32.U)

      val rewardArr = Seq(
        "h0004_0003_0002_0001".U,
        "h0003_0002_0001_0004".U,
        "h0002_0001_0004_0003".U,
        "h0001_0004_0003_0002".U
      )

      c.arrayToMem(0, rewardArr)
      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      var cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)
      c.expectMem(addr = 32, value = 1.U)
      c.expectMem(addr = 40, value = 2.U)
      c.expectMem(addr = 48, value = 3.U)
      c.expectMem(addr = 56, value = 0.U)


      c.expectMem(addr = 64, value = 1.U)
      c.expectMem(addr = 72, value = 1.U)
      c.expectMem(addr = 80, value = 1.U)
      c.expectMem(addr = 88, value = 1.U)


    }
  }


  it should "work on more complicated example" in {
    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      val nAgents = 4
      val nObjects = 5
      val baseAddr = 0
      val baseAddrRes = 1024
      c.writeReg("rfIn_nAgents", nAgents.U)
      c.writeReg("rfIn_nObjects", nObjects.U)
      c.writeReg("rfIn_baseAddr", baseAddr.U)
      c.writeReg("rfIn_baseAddrRes", baseAddrRes.U)

      val rewMat: Seq[Seq[Long]] = Seq(
        Seq(0x0, 0x2, 0x0, 0x0, 0x3),
        Seq(0x7, 0, 0x17, 0, 0),
        Seq(0x11, 0x18, 0, 0, 0),
        Seq(0, 0x6, 0xD, 0x14, 0)
      )

      val rewardArr = generateMemoryArray(rewMat, 16)
      println(rewardArr)
      c.arrayToMem(baseAddr, rewardArr)
      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      var cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)

      val assignments = Seq(0, 2, 1, 3, 0)

      for (j <- 0 until nObjects) {
        c.expectMem(baseAddrRes + 8 * j, assignments(j).U)
      }
      // TODO: check prices
    }
  }


  it should "10x10 with 4PEs" in {
    val ap = new AuctionParams(
      nPEs = 4, bitWidth = 16, memWidth = 64, maxProblemSize = 16
    )

    test(new TesterWrapper({ p => new AuctionBram(p, ap) }, "_dump")) { c =>
      val nAgents = 10
      val nObjects = 10
      val baseAddr = 0
      val baseAddrRes = 1024
      c.writeReg("rfIn_nAgents", nAgents.U)
      c.writeReg("rfIn_nObjects", nObjects.U)
      c.writeReg("rfIn_baseAddr", baseAddr.U)
      c.writeReg("rfIn_baseAddrRes", baseAddrRes.U)

      val rewardArr = generateMemoryArray(rewMatBig, 16)
      println(rewardArr)
      c.arrayToMem(baseAddr, rewardArr)
      c.writeReg("rfIn_start", 1.U)
      c.clock.step(1)
      c.writeReg("rfIn_start", 0.U)

      var cnt = 0
      while (c.readReg("rfOut_finished").litValue != 1 &&
        cnt < 1000) {

        c.clock.step()
        cnt = cnt + 1
      }
      c.expectReg("rfOut_finished", 1.U)

      val assignments = Seq(3,4,6,0,9,8,7,5,1,2)

      for (j <- 0 until nObjects) {
        c.expectMem(baseAddrRes + 8*j, assignments(j).U)
      }
      // TODO: check prices
    }
  }
}
