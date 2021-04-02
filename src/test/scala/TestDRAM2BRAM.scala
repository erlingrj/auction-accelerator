package auction

import Chisel.Cat
import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.stage.PrintFullStackTraceAnnotation
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import firrtl.AnnotationSeq
import fpgatidbits.dma._
import scopt._
import firrtl.annotations._
import fpgatidbits.ocm.OCMMasterIF

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TestDRAM2BRAM extends FlatSpec with ChiselScalatestTester with Matchers {

  val verilator = Seq(VerilatorBackendAnnotation, PrintFullStackTraceAnnotation)

  val mp = new MemReqParams(32, 64, 6, 1, true)
  val ap = new MemCtrlParams(
    nPEs = 8, bitWidth = 8, mrp = mp, maxProblemSize = 64
  )

  def initClocks(c: DRAM2BRAM): Unit = {
    c.io.agentRowAddress.initSink().setSinkClock(c.clock)
    c.io.dramRsp.initSource().setSourceClock(c.clock)
    c.io.dramReq.initSink().setSinkClock(c.clock)
  }

  def arrToDRAMWord(vals: Array[Int]): UInt = {
    var res: Long = 0
    for (i <- 0 until 8) {
      res = res | vals(i).toLong << 8*i
      println(s"i=$i v=${vals(i)} res=$res")
    }
    res.U
  }

  def dramReqs(c: DRAM2BRAM, nRows: Int, nCols: Int, baseAddr: Int): Seq[GenericMemoryRequest] = {
    val nBytes = (math.ceil(nCols.toDouble / 8) * 8 * nRows).toInt
    var nBytesPerReq: mutable.MutableList[Int] = mutable.MutableList()
    var _bytes = nBytes
    while (_bytes > 0) {
      if (_bytes >= 64) {
        nBytesPerReq += 64
        _bytes -= 64
      } else {
        nBytesPerReq += 8
        _bytes -= 8
      }
    }
    val addrs = nBytesPerReq.scanLeft(baseAddr)(_ + _)
    Seq.tabulate(nBytesPerReq.length)(idx =>
      chiselTypeOf(c.io.dramReq).bits.Lit(
        _.isWrite -> false.B,
        _.numBytes -> nBytesPerReq(idx).U,
        _.channelID -> 0.U,
        _.metaData -> 0.U,
        _.addr -> addrs(idx).U
      ))
  }

  def dramRsps(c: DRAM2BRAM, words: Seq[Seq[Int]], nRows: Int, nCols: Int): Seq[GenericMemoryResponse]  = {
    assert(nCols == words(0).length)
    val totBytesPerRow = (math.ceil(nCols.toDouble/8) * 8).toInt
    var els = new Array[Array[Int]](words.length)
    for (i <- 0 until els.length) {
      els(i) = new Array[Int](totBytesPerRow)
      for (j <- 0 until words(i).length) {
        els(i)(j) = words(i)(j)
      }
    }

    val memWordsFlat = els.flatten
    val totNumMemWords = (totBytesPerRow/8) *nRows
    var memWords = new Array[Array[Int]](totNumMemWords)
    var flatIdx = 0
    for (i <- 0 until memWords.length) {
      memWords(i) =new Array[Int](8)
      for (j <- 0 until memWords(i).length) {
        memWords(i)(j) = memWordsFlat(flatIdx)
        flatIdx += 1
      }
    }
    memWords.foreach(v => print(arrToDRAMWord(v)))
    Seq.tabulate(memWords.length)(idx =>
    chiselTypeOf(c.io.dramRsp).bits.Lit(
      _.readData->arrToDRAMWord(memWords(idx)),
      _.metaData->0.U,
      _.isWrite->false.B,
      _.channelID->0.U,
      _.isLast->false.B,
    ))
  }

  def agentRowAddresses(c:DRAM2BRAM, words: Seq[Seq[Int]], nRows: Int, nCols: Int): Seq[AgentRowInfo] = {
    var bramColIdx = 0; var bramRowIdx = 0;
    val agentRows = ArrayBuffer[(Int,Int,Int)]()
    var isRegistered = false
    for (i <- 0 until words.length) {
      for (j <- 0 until words(i).length) {
        if (words(i)(j) != 0) {
          if (!isRegistered) {
            isRegistered = true
            agentRows += ((i,bramColIdx, bramRowIdx))
          }
          bramColIdx += 1
          if (bramColIdx == c.p.nPEs) {
            bramColIdx = 0
            bramRowIdx += 1
          }
        }
      }
      isRegistered = false
    }
    print(agentRows)
    Seq.tabulate(agentRows.length)(idx =>
    chiselTypeOf(c.io.agentRowAddress).bits.Lit(
      _.agentId -> agentRows(idx)._1.U,
      _.colAddr -> agentRows(idx)._2.U,
      _.rowAddr -> agentRows(idx)._3.U

    ))
  }


  def tuple2BramEl(c: DRAM2BRAM, t: (Int,Int,Int)): BigInt = {
    val res = BigInt((t._1).toLong | (t._2).toLong << 1 | t._3.toLong << (1 + c.p.agentWidth))
    res
  }

  def bramEls2BramLine(c: DRAM2BRAM, e: Seq[BigInt]): BigInt = {
    var res: BigInt = 0
    e.zipWithIndex.foreach((v) =>
      res = res | v._1 << (v._2 * (c.p.bitWidth + c.p.agentWidth + 1) )
    )
    res
  }

  def bramCmds(c: DRAM2BRAM, words: Seq[Seq[Int]], nRows: Int, nCols: Int): Seq[BigInt] = {
    def findLastNonZero(v: Seq[Int]): Int = {
      var res = 0
      v.map(_ != 0).zipWithIndex.foreach(v => if(v._1) res = v._2)
      res
    }

    val elsPerRow = c.p.nPEs
    var bramRow = ArrayBuffer[(Int,Int,Int)]() //last, col, value
    val bramRows= ArrayBuffer[ArrayBuffer[(Int,Int,Int)]]()

    for (i <- 0 until words.length) {
      var validInRow = false
      val lastNonZero = findLastNonZero(words(i))
      for (j <- 0 until words(i).length) {
        if (words(i)(j) != 0) {
          validInRow = true
          if (j == lastNonZero) bramRow += (( 1, j, words(i)(j)))
          else bramRow += (( 0, j, words(i)(j)))
        }
        // If we have filled a row. ADd to the bramROws
        if (bramRow.length == elsPerRow) {
          bramRows += bramRow
          bramRow = ArrayBuffer[(Int,Int,Int)]()
        }
        }
      }

    if (bramRow.length > 0) {
      bramRows += bramRow
    }

    Seq.tabulate(bramRows.length)(idx =>
      bramEls2BramLine(c, bramRows(idx).map(tuple2BramEl(c,_)))
    )

    }


  def calcNBramCmds(c: DRAM2BRAM, words: Seq[Seq[Int]]): Int = {
    var nBramCmds = 0
    var runningValids = 0
    words.foreach(_.foreach(
      v => if (v > 0) {
        runningValids += 1
        if (runningValids == c.p.nPEs) {
          nBramCmds += 1
          runningValids = 0
        }
      }
    ))
    if (runningValids > 0) nBramCmds + 1
    else nBramCmds
  }

  def expectBramCmds(c: DRAM2BRAM, words: Seq[BigInt]): Unit = {
    for (i <- 0 until words.length) {
      var cc = 0
      while(c.io.bramCmd.req.writeEn.peek.litToBoolean == false) {
        if (cc >= 1000) assert(false)
        c.clock.step(1)
        cc +=1
      }
      c.io.bramCmd.expect(chiselTypeOf(c.io.bramCmd).Lit(
        _.req.writeEn -> true.B,
        _.req.writeData -> words(i).U,
        _.req.addr -> i.U,
        _.rsp.readData -> 0.U
      ))
    }
  }


  behavior of "DRAM2BRAM"

  it should "correctly transform simple example" in {

  test(new DRAM2BRAM(ap)) { c =>
      initClocks(c)
      val nRows = 2;
      val nCols = 8;
      val baseAddr = 0
      c.io.nRows.poke(nRows.U)
      c.io.nCols.poke(nCols.U)
      c.io.start.poke(true.B)
      c.io.baseAddr.poke(baseAddr.U)

      c.io.dramReq.expectDequeueSeq(dramReqs(c, nRows, nCols, baseAddr))

      val data = Seq(
        Seq(1,0,3,0,5,0,7,0),
        Seq(0,2,0,4,0,6,0,8)
      )
      fork {
        c.io.dramRsp.enqueueSeq(dramRsps(c,data, nRows, nCols))
      }.fork {
        c.io.agentRowAddress.expectDequeueSeq(agentRowAddresses(c,data,nRows,nCols))
      }.fork {
        expectBramCmds(c,bramCmds(c,data,nRows,nCols))
      }.join()
    }
  }
  it should "Initialize correctly" in {
    test(new DRAM2BRAM(ap)) { c =>
      c.io.dramReq.valid.expect(false.B)
      c.io.dramRsp.ready.expect(true.B)
      c.io.agentRowAddress.valid.expect(false.B)
      c.io.finished.expect(false.B)
    }
  }

  it should "generate correct mem reqs" in {
    test(new DRAM2BRAM(ap)) { c =>
      initClocks(c)
      val nRows = 42;
      val nCols = 43;
      val baseAddr = 64
      c.io.nRows.poke(nRows.U)
      c.io.nCols.poke(nCols.U)
      c.io.start.poke(true.B)
      c.io.baseAddr.poke(baseAddr.U)


      c.io.dramReq.expectDequeueSeq(dramReqs(c, nRows, nCols, baseAddr))
    }
  }

}
