package auction

import org.scalatest._
import chiseltest._
import chisel3._
import chisel3.experimental.BundleLiterals._

class TestAuctionController extends FlatSpec with ChiselScalatestTester with Matchers {

  object AuctionTestParams extends AuctionParams {
    val nPEs = 4
    val bitWidth = 32
  }


  behavior of "AuctionController"

  it should "Initialize correctly" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.finished.expect(false.B)
      c.io.streamReaderCtrlSignals.start.expect(false.B)
    }
  }

  it should "Request the correct memory addresses" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.baseAddress.poke(0.U)
      c.io.nCols.poke(4.U)
      c.io.nRows.poke(4.U)

      c.clock.step(4)
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(0.U)

    }
  }


  it should "Request and update correctly" in {
    test(new AuctionController(AuctionTestParams)) { c =>
      c.io.searchResultIn.initSource.setSourceClock(c.clock)
      c.io.baseAddress.poke(0.U)
      c.io.nCols.poke(4.U)
      c.io.nRows.poke(4.U)

      c.clock.step(4)
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(0.U)


      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(true.B)
      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(false.B)
      c.io.searchResultIn.ready.expect(true.B)

      c.io.searchResultIn.enqueueNow(
        chiselTypeOf(c.io.searchResultIn)
          .bits.Lit(_.winner -> 2.U, _.bid -> 69.U)
      )
      c.io.start.poke(true.B)
      c.io.streamReaderCtrlSignals.start.expect(true.B)
      c.io.streamReaderCtrlSignals.byteCount.expect(16.U)
      c.io.streamReaderCtrlSignals.baseAddr.expect(16.U)


      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(true.B)
      c.clock.step(1)
      c.io.streamReaderCtrlSignals.finished.poke(false.B)
      c.io.searchResultIn.ready.expect(true.B)


      c.io.searchResultIn.enqueueNow(
        chiselTypeOf(c.io.searchResultIn)
          .bits.Lit(_.winner -> 0.U, _.bid -> 169.U)
      )
      c.clock.step(1)
    }
  }

}
