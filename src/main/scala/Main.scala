package auction

import java.nio.file.Paths
import chisel3._

import sys.process._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import fpgatidbits.PlatformWrapper._
import fpgatidbits.TidbitsMakeUtils
import fpgatidbits.TidbitsMakeUtils.fileCopy
import fpgatidbits.dma.MemReqParams
import fpgatidbits.ocm.{DualPortBRAM, SimpleDualPortBRAM, SinglePortBRAM}
import fpgatidbits.synthutils.{PrintableParam, VivadoSynth}

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
      nPEs = p_nPEs, bitWidth = p_bitWidth, memWidth = 64, maxProblemSize = p_maxProblemSize,
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
      nPEs = p_nPEs, bitWidth = p_bitWidth, memWidth = 64, maxProblemSize = p_maxProblemSize,
    )

    println(s"Writing verilator to ${targetDir}")
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

  val instFxn_Accountant = {(ap: AssignmentEngineParams) => new AssignmentEngine(ap)}
  val aP = new AssignmentEngineParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64, mrp = mp
  )
  val instFxn_PE = {(ap: ProcessingElementParams) => new ProcessingElement(ap)}
  val peP = new ProcessingElementParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64
  )
  val instFxn_MemoryController = {(ap: MemCtrlParams) => new BramController(ap)}
  val mcP = new MemCtrlParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64, mrp = mp
  )
  val instFxn_SearchTask= {(ap: SearchTaskParams) => new SearchTask(ap)}
  val stP = new SearchTaskParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64
  )
  val instFxn_DataDistributor = {(ap: DataMuxParams) => new DataMux(ap)}
  val ddP = new DataMuxParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64, memWidth = 64
  )
  val instFxn_Controller= {(ap: ApplicationControllerParams) => new ApplicationController(ap)}
  val cP = new ApplicationControllerParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64, mrp = mp
  )

  val instFxn_DRAM2BRAM= {(ap: MemCtrlParams) => new Dram2Bram(ap)}


  val instFxn_Auction= {(ap: AuctionParams) => new Auction(ZedBoardParams, ap)}
  val auctionP = new AuctionParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64, memWidth = 64
  )

  // Just adding auctionParams there so it adheres to the needing a PrintableParam in
  val instFxn_SinglePortBRAM= { (ap:AssignmentEngineParams) => new SinglePortBRAM(8,128)}
  val instFxn_SimpleDualPortBRAM= { (ap:AssignmentEngineParams) => new SimpleDualPortBRAM(1,512)}

  val instFxn_BramStore = {(p: BramStoreParams) => new BramStore(p)}
  val bsp = new BramStoreParams(
    bitWidth = 8, nPEs = 8, maxProblemSize = 64
  )

  val instFxn_RegStore = {(p: RegStoreParams) => new RegStore(gen=0.U(8.W), p)}
  val rsp = new RegStoreParams (
    nReadPorts = 9, nWritePorts = 1, nReadWritePorts = 0, addrBits = 8
  )

  def main(args: Array[String]): Unit = {
    val chName: String = args(0)
    val chPath: String = args(1)
    val platform: String = args(2)
    val chLog: String = chName + ".log"
    val fpgaPart: String = TidbitsMakeUtils.fpgaPartMap(platform)

    if (chName == "Accountant") {
      VivadoSynth.characterizePoint(aP, instFxn_Accountant, chPath, fpgaPart, "Accountant")
    } else if (chName == "CharacterizeProcessingElement") {
      VivadoSynth.characterizePoint(peP, instFxn_PE, chPath, fpgaPart, "ProcessingElement")
    } else if (chName == "CharacterizeMemoryController") {
      VivadoSynth.characterizePoint(mcP, instFxn_MemoryController, chPath, fpgaPart, "BramController")
    } else if (chName == "CharacterizeSearchTask") {
      VivadoSynth.characterizePoint(stP, instFxn_SearchTask, chPath, fpgaPart, "SearchTree")
    } else if (chName == "CharacterizeDataDistributor") {
      VivadoSynth.characterizePoint(ddP, instFxn_DataDistributor, chPath, fpgaPart, "DataDistributor")
    } else if (chName == "CharacterizeSimpleDualPortBRAM") {
      VivadoSynth.characterizePoint(aP, instFxn_SimpleDualPortBRAM, chPath, fpgaPart, "SimpleDualPortBRAM")
    } else if (chName == "CharacterizeSinglePortBRAM") {
      VivadoSynth.characterizePoint(aP, instFxn_SinglePortBRAM, chPath, fpgaPart, "SinglePortBRAM")
    } else if (chName == "CharacterizeBramStore") {
      VivadoSynth.characterizePoint(bsp, instFxn_BramStore, chPath, fpgaPart, "BramStore")
    } else if (chName == "CharacterizeController") {
      VivadoSynth.characterizePoint(cP, instFxn_Controller, chPath, fpgaPart, "ApplicationController")
    } else if (chName == "CharacterizeDRAM2BRAM") {
      VivadoSynth.characterizePoint(mcP, instFxn_DRAM2BRAM, chPath, fpgaPart, "Dram2Bram")
    } else if (chName == "CharacterizeAuction") {
      VivadoSynth.characterizePoint(auctionP, instFxn_Auction, chPath, fpgaPart, "Auction")
    } else if (chName == "CharacterizeAll") {
      VivadoSynth.characterizePoint(peP, instFxn_PE, chPath, fpgaPart, "ProcessingElement")
      VivadoSynth.characterizePoint(mcP, instFxn_MemoryController, chPath, fpgaPart, "BramController")
      VivadoSynth.characterizePoint(stP, instFxn_SearchTask, chPath, fpgaPart, "SearchTree")
      VivadoSynth.characterizePoint(ddP, instFxn_DataDistributor, chPath, fpgaPart, "DataDistributor")
      VivadoSynth.characterizePoint(cP, instFxn_Controller, chPath, fpgaPart, "ApplicationController")
      VivadoSynth.characterizePoint(mcP, instFxn_DRAM2BRAM, chPath, fpgaPart, "Dram2Bram")
      VivadoSynth.characterizePoint(aP, instFxn_Accountant, chPath, fpgaPart, "Accountant")
      VivadoSynth.characterizePoint(auctionP, instFxn_Auction, chPath, fpgaPart, "Auction")

    }
  }
}


object DriverMain {

}
