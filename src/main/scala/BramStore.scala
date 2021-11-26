package auction

import auction.RegStoreParams
import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams
import fpgatidbits.ocm.SimpleDualPortBRAM
import fpgatidbits.synthutils.PrintableParam

class BramStoreParams(
  val bitWidth: Int,
  val nPEs: Int,
  val maxProblemSize: Int,
) extends PrintableParam {

  override def headersAsList(): List[String] = {
    List(

    )
  }

  override def contentAsList(): List[String] = {
    List(

    )
  }
  def agentWidth = log2Ceil(maxProblemSize)
}

class BramStoreIO(p: BramStoreParams) extends Bundle {
  val prices = Vec(p.nPEs, Output(UInt(p.bitWidth.W)))
  val idxs = Flipped(Decoupled(Vec(p.nPEs, Input(UInt(p.agentWidth.W)))))

  val accReadAddr = Input(UInt(p.agentWidth.W))
  val accReadData = Output(UInt(p.bitWidth.W))

  val accWriteAddr = Input(UInt(p.agentWidth.W))
  val accWriteData = Input(UInt(p.bitWidth.W))
  val accWriteDataValid = Input(Bool())

  val dump = Input(Bool())
  val dumpOut = Decoupled(UInt(p.bitWidth.W))

}

class BramStoreWord(p: BramStoreParams) extends Bundle {
  val prices = Vec(p.maxProblemSize, UInt(p.bitWidth.W))
}


class BramStore(p: BramStoreParams) extends MultiIOModule {

  val io = IO(new BramStoreIO(p))

  val memDataBits = p.maxProblemSize * p.bitWidth

  val mem = Module(new SimpleDualPortBRAM(1, memDataBits))


  mem.io.write.req.writeEn := false.B
  mem.io.write.req.addr := DontCare
  mem.io.write.req.writeData := DontCare
  mem.io.read.req.writeEn := false.B
  mem.io.read.req.writeData := DontCare
  io.dumpOut.valid := false.B
  io.dumpOut.bits := DontCare

  // Each cycle we read from the memory
  val regMemRead = RegInit(0.U.asTypeOf(new BramStoreWord(p)))
  val memRead = WireInit(0.U.asTypeOf(new BramStoreWord(p)))

  // Also the indexes are latched
  val regIdxs = RegInit(VecInit(Seq.fill(p.nPEs)(0.U(p.bitWidth.W))))
  io.idxs.ready := true.B
  when (io.idxs.valid) {
    regIdxs zip io.idxs.bits map {
      case (l, r) =>
        l := r
    }
  }

  mem.io.read.req.addr := 0.U
  memRead := mem.io.read.rsp.readData.asTypeOf(new BramStoreWord(p))

  // TODO: There is somee structure here meaning that we should loop through the memRead.prices instead
  for (i <- 0 until p.nPEs) {
    io.prices(i) := memRead.prices(regIdxs(i))
  }


  // Do accountant reads
  val regAccWriteAddr= RegInit(0.U(p.agentWidth.W))
  val regAccWriteData = RegInit(0.U(p.bitWidth.W))

  val accRead = Wire(UInt(p.bitWidth.W))
  accRead := memRead.prices(io.accReadAddr)
  val forward = io.accReadAddr === regAccWriteAddr && RegNext(io.accWriteDataValid)

  // Do accountant writes
  when(io.accWriteDataValid) {
    val memWrite = WireInit(memRead)
    memWrite.prices(io.accWriteAddr) := io.accWriteData
    mem.io.write.req.writeEn := true.B
    mem.io.write.req.writeData := memWrite.asUInt()

    regAccWriteAddr := io.accWriteAddr
    regAccWriteData := io.accWriteData
  }

  when (forward) {
    io.accReadData := regAccWriteData
  }.otherwise{
    io.accReadData := accRead
  }

  val dumpCnt = RegInit(0.U(p.agentWidth.W))
  when (io.dump) {
    io.dumpOut.bits := memRead.prices(dumpCnt)
    io.dumpOut.valid := true.B
    when (io.dumpOut.fire()) {
      dumpCnt := dumpCnt + 1.U
    }
  }

  when (reset.asBool()) {
    mem.io.write.req.writeEn := true.B
    mem.io.write.req.writeData := 0.U
  }
}
