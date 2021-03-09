package auction

import java.nio.file.Paths

import chisel3._

import sys.process._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import fpgatidbits.PlatformWrapper._
import fpgatidbits.TidbitsMakeUtils
import fpgatidbits.TidbitsMakeUtils.fileCopy


object Settings {
  type AccelInstFxn = PlatformWrapperParams ⇒ GenericAccelerator
  type AccelMap = Map[String, AccelInstFxn]

  def makeInstFxn(myP: AuctionParams) : AccelInstFxn = {
    return { (p: PlatformWrapperParams) => new Auction(p, myP)}
  }

  def writeVerilogToFile(verilog: String, path: String) = {
    import java.io._
    val fname = path
    val f = new File(fname)
    if (!f.exists()) {
      f.getParentFile.mkdirs
      f.createNewFile()
    }
    val writer = new PrintWriter(f)
    writer.write(verilog)
    writer.close()

  }

}

object ChiselMain {
  def main(args: Array[String]): Unit = {
    val platformName: String = args(0)
    val targetDir: String = args(1)
    val p_nPEs: Int = args(2).toInt
    val p_bitWidth: Int = args(3).toInt
    val p_maxProblemSize: Int = args(4).toInt

    object auctionParams extends AuctionParams {
      val nPEs = p_nPEs
      val bitWidth = p_bitWidth
      val memWidth = 64
      val maxProblemSize = p_maxProblemSize
    }

    val platformInst = TidbitsMakeUtils.platformMap(platformName)
    val accInst = Settings.makeInstFxn(auctionParams)
    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst, targetDir))
    Settings.writeVerilogToFile(verilogString, targetDir + "auction.v")
  }
}

object VerilatorMain {
  def main(args: Array[String]): Unit = {
    val targetDir: String = args(0)
    val p_nPEs: Int = args(1).toInt
    val p_bitWidth: Int = args(2).toInt
    val p_maxProblemSize: Int = args(3).toInt

    object auctionParams extends AuctionParams {
      val nPEs = p_nPEs
      val bitWidth = p_bitWidth
      val memWidth = 64
      val maxProblemSize = p_maxProblemSize
    }

    val platformInst = {f => new VerilatedTesterWrapper(f, targetDir)}
    val accInst = Settings.makeInstFxn(auctionParams)
    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst))
    Settings.writeVerilogToFile(verilogString, targetDir + "TesterWrapper.v")

    // Copy example test program
    val resRoot = Paths.get("src/main/resources").toAbsolutePath
    fileCopy(s"$resRoot/AuctionVerilatorTest.cpp", targetDir + "/main.cpp")

  }
}

object DriverMain {

}