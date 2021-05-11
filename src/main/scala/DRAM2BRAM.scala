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

class DRAM2BRAMIO(val p: MemCtrlParams) extends Bundle {
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
class DRAM2BRAM(val p: MemCtrlParams) extends Module {
  val io = IO(new DRAM2BRAMIO(p))
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

  val regBytesLeftInRow = RegInit(0.U(32.W))


  val sIdle :: sParseDramWord :: sProduceBramLine :: sLastSendOff ::Nil = Enum(4)
  val regState = RegInit(sIdle)
  val regBramLine = RegInit(0.U.asTypeOf(new BramLine(p)))
  val regBramElsLeftOvers = RegInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))

  val regElCnt = RegInit(0.U(log2Ceil(p.nPEs).W))
  val regRowAddrCnt = RegInit(0.U(log2Ceil(p.bramAddrWidth).W))
  val regColAddrCnt = RegInit(0.U(log2Ceil(p.bramAddrWidth).W))
  val regAgentRowCnt = RegInit(0.U((p.agentWidth+1).W))
  val regBramRowCnt = RegInit(0.U(log2Ceil(p.bramAddrWidth).W))

  val regNRows = RegInit(0.U((p.agentWidth.W)))
  val regNCols = RegInit(0.U((p.agentWidth.W)))
  val regNBytesInRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowsPerMatrixRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowIdx = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regNRowsReceived = RegInit(0.U(p.agentWidth.W)) // Count how many rows we have fully recevied from DRAM

  val regAgentRowInfo = RegInit(0.U.asTypeOf(new AgentRowInfo(p)))
  val regAgentRowInfoAgentIdx = RegInit(0.U(p.agentWidth.W))
  val regAgentHasValid = RegInit(false.B)



  val s1_regBramEls = RegInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
  val s1_rowFinished = RegInit(false.B)
  val s1_rowStarted = RegInit(false.B)


  // State machine:

  // After reciving a word from memory we have to:
  // 1. Split it up into n elements
  // 2. Create BramEls out of non-zero elements
  // 3. Add them to the BramLine which builds up over multiple iterations
  // There are a couple of scenarios
  // A. Some of the BramEl represents the first element in a row =>
  //  We add that information to the appropriate element of the RowAddr
  // B. This mem-row fills up a bram-line
  //  We fire of the bram-line and next cycle we finish with the left-overs (not accepting new data from mem)
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
//        val bytesInRow = (io.nCols >> ((p.bitWidth>>3)-1))
        val bytesInRow = RoundUpAlign(align=8, io.nCols * (p.bitWidth/8).U)
        regNBytesInRow := bytesInRow
        regBytesLeftInRow := bytesInRow

        regState := sParseDramWord

        regBramRowCnt := 0.U
        regNCols := io.nCols
        regNRows := io.nRows
        regNRowsReceived := 0.U
        regDramRowIdx := 0.U
        regElCnt := 0.U
        regBramLine := 0.U.asTypeOf(new BramLine(p))
        regAgentRowCnt := 0.U
        regColAddrCnt := 0.U
      }
    }
    is (sParseDramWord) {
      // Read out and parse a word from DRAM into a Vec of BramEls with col-info
      rspQ.deq.ready := true.B
      when (rspQ.deq.fire()) {
        val dramRow = rspQ.deq.bits.readData
        val bramEls = WireInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
        bramEls.zipWithIndex.map { case (b, i) =>
          when(regBytesLeftInRow > i.U) {
            b.value := dramRow((i + 1) * p.bitWidth - 1, i * p.bitWidth)
          }
        }
        when(regBytesLeftInRow === regNBytesInRow) {
          s1_rowStarted := true.B
        }.otherwise {
          s1_rowStarted := false.B
        }

        when(regBytesLeftInRow > (p.mrp.dataWidth/8).U) {
          regBytesLeftInRow := regBytesLeftInRow - (p.mrp.dataWidth/8).U // Doenst matter if it overflows the last round
          regColAddrCnt := regColAddrCnt + (64/p.bitWidth).U
          s1_rowFinished := false.B
        }.otherwise {
          s1_rowFinished := true.B
          regBytesLeftInRow := regNBytesInRow
          regColAddrCnt := 0.U
          regNRowsReceived := regNRowsReceived + 1.U
        }
        bramEls.zipWithIndex.map{case (b,i) =>
          val col = WrapAdd(regColAddrCnt,i.U, regNCols)
          b.col := col
        }
        s1_regBramEls := bramEls

        regState := sProduceBramLine
      }.elsewhen(regNRowsReceived === regNRows) {
        // We dont get anymore lines since we got'em all. Send out the stored BRAM-line if a
        when(regElCnt > 0.U) {
          val bramLine = WireInit(0.U.asTypeOf(new BramLine(p)))
          bramLine.els.zipWithIndex.map { case (el, i) =>
            when (i.U < regElCnt) {
              el := regBramLine.els(i)
            }
          }
          io.bramCmd.req.writeEn := true.B
          io.bramCmd.req.writeData := bramLine.asUInt()
          io.bramCmd.req.addr := regBramRowCnt


          // Also send out the RowAgentInfo
          io.agentRowAddress.req.valid := regAgentHasValid
          io.agentRowAddress.req.bits.addr := regAgentRowInfoAgentIdx
          io.agentRowAddress.req.bits.wdata:= regAgentRowInfo
        }
        io.finished := true.B
        regState := sIdle
      }
    }
    is (sProduceBramLine) {

      val nValids = PopCount(s1_regBramEls.map(_.value =/= 0.U))
      val valids = Compactor(s1_regBramEls, s1_regBramEls.map(_.value =/= 0.U))
      val newRegElCnt = Cat(0.U(1.W), regElCnt) + nValids
      regState := sParseDramWord

      when(s1_rowStarted) {
        // One-round business. Row is also finished
        when (s1_rowFinished){
          when(nValids > 0.U) {
            io.agentRowAddress.req.valid := true.B
            io.agentRowAddress.req.bits.wen := true.B
            io.agentRowAddress.req.bits.addr := regAgentRowCnt
            // Spread out over 2 or 1 BRAM row?
            when (newRegElCnt > p.nPEs.U) {
              io.agentRowAddress.req.bits.wdata := AgentRowInfo(p, regBramRowCnt, 2.U)
            }.otherwise {
              io.agentRowAddress.req.bits.wdata := AgentRowInfo(p, regBramRowCnt,1.U)
            }
          }
        }.otherwise {
          // Multi-round thing
          regAgentRowInfo.rowAddr := regBramRowCnt
          regAgentRowInfoAgentIdx := regAgentRowCnt
          regAgentHasValid := (nValids > 0.U)
          when (newRegElCnt >= p.nPEs.U) {
            regAgentRowInfo.length := 2.U
          }.otherwise {
            regAgentRowInfo.length := 1.U
          }
        }
      }.elsewhen(s1_rowFinished) {
        // This round wraps up for a row
        when (regAgentHasValid) {
          io.agentRowAddress.req.valid := true.B
          io.agentRowAddress.req.bits.addr := regAgentRowInfoAgentIdx
          when (newRegElCnt > p.nPEs.U) {
            io.agentRowAddress.req.bits.wdata := AgentRowInfo(p, regAgentRowInfo.rowAddr, regAgentRowInfo.length + 1.U)
          }.otherwise {
            io.agentRowAddress.req.bits.wdata := regAgentRowInfo
          }
        }.otherwise {
          io.agentRowAddress.req.bits.addr := regAgentRowInfoAgentIdx
          io.agentRowAddress.req.valid := (nValids > 0.U)
          io.agentRowAddress.req.bits.wdata := AgentRowInfo(p, regAgentRowInfo.rowAddr, 1.U)
        }
      }.otherwise{
        // Intermediate round. Check if we got valid stuff there
        when (!regAgentHasValid) {
          regAgentHasValid := (nValids > 0.U)
        }
      }

      when (newRegElCnt >= p.nPEs.U) {
        // We are filling up a bramLine. Fire it of and fit the remainder on the next line
        // 1. fill the remaining spots
        val bramLine = WireInit(0.U.asTypeOf(new BramLine(p)))
        val spotsLeft = p.nPEs.U - regElCnt
        bramLine.els.zipWithIndex.map { case (el, i) =>
          when(i.U < regElCnt) {
            el := regBramLine.els(i)
          }.otherwise{
            el := valids(i.U-regElCnt)
          }
        }
        // Fire it off
        io.bramCmd.req.writeEn := true.B
        io.bramCmd.req.writeData := bramLine.asUInt()
        io.bramCmd.req.addr := regBramRowCnt

        // Add the remaining to the regBramLine
        valids.zipWithIndex.map{case (el,i) =>
          when(i.U>=spotsLeft) {
            regBramLine.els(i.U-spotsLeft) := el
          }
        }
        // Increment the idx
        regElCnt := nValids - spotsLeft
        regBramRowCnt := regBramRowCnt + 1.U

        // Also increment the length field of the agentRowInfo register
        when (!s1_rowFinished && !s1_rowStarted) {
          regAgentRowInfo.length := regAgentRowInfo.length + 1.U
        }

        // If this was the last dram-word go to lastSendOff
        when (s1_rowFinished && newRegElCnt > p.nPEs.U) {
          regState := sLastSendOff
        }

      }.elsewhen(s1_rowFinished && ( (nValids > 0.U) || regElCnt > 0.U) ) {
        // This was the last round (and first)
        // Send off and reset
        val bramLine = WireInit(0.U.asTypeOf(new BramLine(p)))
        for (i <- 0 until bramLine.els.length)  {
          when (i.U < regElCnt) {
            bramLine.els(i) := regBramLine.els(i)
          }.otherwise {
            if (i < 64/p.bitWidth) {
              bramLine.els(i) := valids(i.U - regElCnt)
            }
          }
        }

        // Fire it off
        io.bramCmd.req.writeEn := true.B
        io.bramCmd.req.writeData := bramLine.asUInt()
        io.bramCmd.req.addr := regBramRowCnt

        regElCnt := 0.U
        regBramRowCnt := regBramRowCnt + 1.U

      }.otherwise{
        // Add the valid guys to the bramLine and increment
        valids.zipWithIndex.map{case (el, i) =>
          when(i.U < nValids) {
            regBramLine.els(regElCnt + i.U) := el
          }
        }
        // Increment the idx
        regElCnt := newRegElCnt
        regState := sParseDramWord
      }

      when (s1_rowFinished) {
        regAgentRowCnt := regAgentRowCnt + 1.U
      }

    }
    is (sLastSendOff) {
      io.bramCmd.req.writeEn := true.B
      io.bramCmd.req.writeData := regBramLine.asUInt()
      io.bramCmd.req.addr := regBramRowCnt

      regBramRowCnt := regBramRowCnt + 1.U
      regElCnt := 0.U

      when (regNRowsReceived === regNRows) {
        regState := sIdle
        io.finished := true.B
      }.otherwise{
        regState := sParseDramWord
      }

    }
  }
}
