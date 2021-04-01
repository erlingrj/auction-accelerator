package auction

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

  /*
  def bramCmds(c: DRAM2BRAM, words: Seq[Seq[Int]], nRows: Int, nCols: Int): Seq[OCMMasterIF] = {
    Seq.tabulate(1)(idx =>
    chiselTypeOf(c.io.bramCmd).Lit(
    )
  }
*/

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
