package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.{GenericMemoryRequest, GenericMemoryResponse, ReadReqGen, RoundUpAlign}
import fpgatidbits.ocm.{FPGAQueue, OCMMasterIF}



class BramLine(val mp: MemCtrlParams) extends Bundle {
  val els = Vec(mp.nPEs, new BramEl(mp))
}


class BramEl(val mp: MemCtrlParams) extends Bundle {
  val value = UInt(mp.bitWidth.W)
  val col = UInt(mp.agentWidth.W)
}

class AgentRowInfo(val p: MemCtrlParams) extends Bundle {
  val rowAddr = UInt(log2Ceil(p.bramDataWidth).W)
  val length = UInt(p.agentWidth.W)
}
object AgentRowInfo {
  def apply(p: MemCtrlParams, rowAddr: UInt, length: UInt): AgentRowInfo = {
    val res = Wire(new AgentRowInfo(p))
    res.rowAddr := rowAddr
    res.length := length
    res
  }
}

class Dram2BramIO(val p: MemCtrlParams) extends Bundle {
  // Interface to DRAM
  val dramReq = Decoupled(new GenericMemoryRequest(p.mrp))
  val dramRsp = Flipped(Decoupled(new GenericMemoryResponse(p.mrp)))

  // Interface to BRAM
  val bramCmd = new OCMMasterIF(p.bramDataWidth, p.bramDataWidth, p.bramAddrBits)

  // Control interface
  val start = Input(Bool())
  val finished = Output(Bool())
  val baseAddr = Input(UInt(64.W))
  val nRows = Input(UInt(log2Ceil(p.maxProblemSize).W))
  val nCols = Input(UInt(log2Ceil(p.maxProblemSize).W))

  // Interface to module storing the agentRowAddresses
  // We need nPEs because worst case each memory fetch leads to nPE rows.
  val agentRowAddress = new RegStoreTransaction(new AgentRowInfo(p),p.agentRowStoreParams)


  def driveDefaults() = {
    dramReq.valid := false.B
    dramReq.bits := DontCare
    dramRsp.ready := false.B
    bramCmd := DontCare
    bramCmd.req.writeEn := false.B
    agentRowAddress.req.bits := DontCare
    agentRowAddress.req.valid := false.B
    agentRowAddress.rsp.ready := false.B
    finished := false.B
  }
}


// Horrible class. Dont even bother looking
class Dram2Bram(val p: MemCtrlParams) extends Module {
  val io = IO(new Dram2BramIO(p))
  io.driveDefaults()
  require(p.bitWidth >= 8)
  //require(p.nPEs >= 8)
  // read request generator
  val rg = Module(new ReadReqGen(p.mrp, chanID=0, maxBeats=8))
  io.dramReq <> rg.io.reqs
  rg.io.ctrl.throttle := DontCare
  rg.io.ctrl.baseAddr := DontCare
  rg.io.ctrl.byteCount := DontCare
  rg.io.ctrl.start := false.B
  // FIFO for reciving DRAM memory data
  val rspQ = Module(new Queue(new GenericMemoryResponse(p.mrp), 64/p.bitWidth)).io
  io.dramRsp <> rspQ.enq
  rspQ.deq.ready := false.B


  val bw = log2Ceil(p.maxProblemSize*p.maxProblemSize)
  val regBramAddr = RegInit(0.U(bw.W))
  val regTotWords = RegInit(0.U(bw.W))
  val regBytesInRow = RegInit(0.U(p.agentWidth.W))
  val regBytesLeftInRow = RegInit(0.U(p.agentWidth.W))
  val regNCols = RegInit(0.U(log2Ceil(p.maxProblemSize).W))
  val regNRows = RegInit(0.U(log2Ceil(p.maxProblemSize).W))


  val regColAddrCnt = RegInit(0.U(p.agentWidth.W))


  val sIdle :: sParseDramWord :: sProduceBramLine ::Nil = Enum(3)
  val regState = RegInit(sIdle)
  val regBramLine = RegInit(0.U.asTypeOf(new BramLine(p)))

  val s1_regBramEls = RegInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
  val s1_regValids = RegInit(VecInit(Seq.fill(64/p.bitWidth)(false.B)))


  // If we dont have
  switch (regState) {
    is (sIdle) {
      rg.io.ctrl.start := false.B
      io.finished := false.B
      when (io.start) {
        rg.io.ctrl.throttle := false.B
        rg.io.ctrl.baseAddr := io.baseAddr
        val byteCount = CalcNBytes(io.nRows, io.nCols, p.bitWidth)
        rg.io.ctrl.byteCount := byteCount
        rg.io.ctrl.start := true.B
        val bytesInRow = io.nCols * (p.bitWidth/8).U

        regNRows := io.nRows
        regNCols := io.nCols
        regColAddrCnt := 0.U
        regBytesInRow := bytesInRow
        regBytesLeftInRow := bytesInRow
        regTotWords := byteCount/8.U
        regState := sParseDramWord
        regBramLine := 0.U.asTypeOf(new BramLine(p))
      }
    }
    is (sParseDramWord) {
      // Read out and parse a word from DRAM into a Vec of BramEls with col-info
      rspQ.deq.ready := true.B
      when(rspQ.deq.fire()) {
        val dramRow = rspQ.deq.bits.readData
        val bramEls = WireInit(VecInit(Seq.fill(64 / p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
        bramEls.zipWithIndex.map { case (b, i) =>
          when(regBytesLeftInRow > i.U) {
            b.value := dramRow((i + 1) * p.bitWidth - 1, i * p.bitWidth)
            s1_regValids(i) := true.B
          }.otherwise {
            s1_regValids(i) := false.B
          }
        }

        when(regBytesLeftInRow > (p.mrp.dataWidth / 8).U) {
          regBytesLeftInRow := regBytesLeftInRow - (p.mrp.dataWidth / 8).U // Doenst matter if it overflows the last round
          regColAddrCnt := regColAddrCnt + 8.U
        }.otherwise {
          regBytesLeftInRow := regBytesInRow
          regColAddrCnt := 0.U
        }
        bramEls.zipWithIndex.map { case (b, i) =>
          val col = regColAddrCnt + i.U
          when(col < regNCols) {
            b.col := col
          }.otherwise {
            b.col := 0.U
            b.value := 0.U
          }
        }
        s1_regBramEls := bramEls

        regState := sProduceBramLine
      }
    }
    is (sProduceBramLine) {


      // Fire it off
      io.bramCmd.req.writeEn := true.B
      io.bramCmd.req.writeData := s1_regBramEls.asUInt()
      io.bramCmd.req.addr := regBramAddr

      regBramAddr := regBramAddr + 1.U

      when(regBramAddr === regTotWords - 1.U) {
        regState := sIdle
        io.finished := true.B
      }.otherwise {
        regState := sParseDramWord
      }
    }
  }
}
