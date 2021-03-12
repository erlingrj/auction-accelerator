package auction

import java.nio.file.Paths

import chisel3._

import sys.process._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import fpgatidbits.PlatformWrapper._
import fpgatidbits.TidbitsMakeUtils
import fpgatidbits.TidbitsMakeUtils.fileCopy
import fpgatidbits.dma.MemReqParams
import fpgatidbits.synthutils.VivadoSynth

// This is heavily inspired by the BISMO project by Yaman Urumgorou BSD License by NTNU


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

    val ap = new AuctionParams(
      nPEs = p_nPEs, bitWidth = p_bitWidth, memWidth = 64, maxProblemSize = p_maxProblemSize
    )

    val platformInst = TidbitsMakeUtils.platformMap(platformName)
    val accInst = Settings.makeInstFxn(ap)
    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst, targetDir))
    Settings.writeVerilogToFile(verilogString, targetDir + "/auction.v")
  }
}

object VerilatorMain {
  def main(args: Array[String]): Unit = {
    val targetDir: String = args(0)
    val p_nPEs: Int = args(1).toInt
    val p_bitWidth: Int = args(2).toInt
    val p_maxProblemSize: Int = args(3).toInt

    val ap = new AuctionParams(
      nPEs = 4, bitWidth = 8, memWidth = 64, maxProblemSize = 16
    )

    val platformInst = {f => new VerilatedTesterWrapper(f, targetDir)}
    val accInst = Settings.makeInstFxn(ap)
    val verilogString = (new chisel3.stage.ChiselStage).emitVerilog(platformInst(accInst))
    Settings.writeVerilogToFile(verilogString, targetDir + "/TesterWrapper.v")

    // Copy example test program
    val resRoot = Paths.get("src/main/resources").toAbsolutePath
    fileCopy(s"$resRoot/AuctionVerilatorTest.cpp", targetDir + "/main.cpp")

  }
}


object CharacterizeMain {


  val mp = new MemReqParams(32, 64, 6, 1, true)

  val instFxn_Accountant = {(ap: AccountantParams) => new AccountantNonPipelined(ap)}
  val aP = new AccountantParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128, mrp = mp
  )
  val instFxn_PE = {(ap: ProcessingElementParams) => new ProcessingElementPar(ap, 0)}
  val peP = new ProcessingElementParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128
  )
  val instFxn_MemoryController = {(ap: MemCtrlParams) => new AuctionDRAMController(ap)}
  val mcP = new MemCtrlParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128, mrp = mp
  )
  val instFxn_SearchTask= {(ap: SearchTaskParams) => new SearchTaskPar(ap)}
  val stP = new SearchTaskParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128
  )
  val instFxn_DataDistributor = {(ap: DataDistributorParams) => new DataDistributorParUnO(ap)}
  val ddP = new DataDistributorParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128, memWidth = 64
  )
  val instFxn_Controller= {(ap: ControllerParams) => new Controller(ap)}
  val cP = new ControllerParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 128, mrp = mp
  )



  def main(args: Array[String]): Unit = {
    val chName: String = args(0)
    val chPath: String = args(1)
    val platform: String = args(2)
    val chLog: String = chName + ".log"
    val fpgaPart: String = TidbitsMakeUtils.fpgaPartMap(platform)

    if (chName == "Accountant") {
      VivadoSynth.characterizePoint(aP, instFxn_Accountant, chPath, fpgaPart, "AccountantNonPipelined")
    }
    else if (chName == "ProcessingElement") {
      VivadoSynth.characterizePoint(peP, instFxn_PE, chPath, fpgaPart, "ProcessingElementPar")
    } else if (chName == "MemoryController") {
      VivadoSynth.characterizePoint(mcP, instFxn_MemoryController, chPath, fpgaPart, "AuctionDRAMController")
    } else if (chName == "SearchTask") {
      VivadoSynth.characterizePoint(stP, instFxn_SearchTask, chPath, fpgaPart, "SearchTaskPar")
    } else if (chName == "DataDistributor") {
      VivadoSynth.characterizePoint(ddP, instFxn_DataDistributor, chPath, fpgaPart, "DataDistributorParUnO")
    } else if (chName == "Controller") {
      VivadoSynth.characterizePoint(cP, instFxn_Controller, chPath, fpgaPart, "Controller")
    } else if (chName == "All") {

      VivadoSynth.characterizePoint(peP, instFxn_PE, chPath, fpgaPart, "ProcessingElementPar")
      VivadoSynth.characterizePoint(mcP, instFxn_MemoryController, chPath, fpgaPart, "AuctionDRAMController")
      VivadoSynth.characterizePoint(stP, instFxn_SearchTask, chPath, fpgaPart, "SearchTaskPar")
      VivadoSynth.characterizePoint(ddP, instFxn_DataDistributor, chPath, fpgaPart, "DataDistributorParUnO")
      VivadoSynth.characterizePoint(cP, instFxn_Controller, chPath, fpgaPart, "Controller")
    }
  }
}


object DriverMain {

}
