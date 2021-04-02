package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.{GenericMemoryRequest, GenericMemoryResponse, ReadReqGen}
import fpgatidbits.ocm.{FPGAQueue, OCMMasterIF}



class BramLine(val mp: MemCtrlParams) extends Bundle {
  val els = Vec(mp.nPEs, new BramEl(mp))
}


class BramEl(val mp: MemCtrlParams) extends Bundle {
  val value = UInt(mp.bitWidth.W)
  val col = UInt(mp.agentWidth.W)
  val last = Bool()
}

class AgentRowInfo(val agentBits: Int, val bramDataBits: Int) extends Bundle {
  val agentId = UInt(agentBits.W)
  val rowAddr = UInt(bramDataBits.W)
  val colAddr = UInt(bramDataBits.W)
}

class DRAM2BRAMIO(val p: MemCtrlParams) extends Bundle {
  // Interface to DRAM
  val dramReq = Decoupled(new GenericMemoryRequest(p.mrp))
  val dramRsp = Flipped(Decoupled(new GenericMemoryResponse(p.mrp)))

  // Interface to BRAM
  val bramCmd = new OCMMasterIF(p.bramDataWidth, p.bramDataWidth, p.bramAddrWidth)

  // Control interface
  val start = Input(Bool())
  val finished = Output(Bool())
  val baseAddr = Input(UInt(64.W))
  val nRows = Input(UInt(log2Ceil(p.maxProblemSize).W))
  val nCols = Input(UInt(log2Ceil(p.maxProblemSize).W))

  // Interface to module storing the agentRowAddresses
  // We need nPEs because worst case each memory fetch leads to nPE rows.
  val agentRowAddress = Decoupled(new AgentRowInfo(log2Ceil(p.maxProblemSize), log2Ceil(p.bramDataWidth)))

  def driveDefaults() = {
    dramReq.valid := false.B
    dramReq.bits := DontCare
    dramRsp.ready := false.B
    bramCmd := DontCare
    bramCmd.req.writeEn := false.B
    agentRowAddress.bits := DontCare
    agentRowAddress.valid := false.B
    finished := false.B
  }
}

class DRAM2BRAM(val p: MemCtrlParams) extends Module {
  val io = IO(new DRAM2BRAMIO(p))
  io.driveDefaults()
  require(p.bitWidth >= 8)
  require(p.nPEs >= 8)
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

  // Messy function that does several things:
  // 1. Splits DRAM Line into BramEls vector (also masks out unused bits due to 64 bit alignment of rows)
  // 2. Also deals with the global regBytesLeftInRow register
  def dramRowToBramEls(dramRow: UInt): Vec[BramEl] = {
    val bramEls = WireInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))
    bramEls.zipWithIndex.map { case (b, i) =>
      when(regBytesLeftInRow > i.U) {
        b.value := dramRow((i + 1) * p.bitWidth - 1, i * p.bitWidth)
      }
    }
    when(regBytesLeftInRow === regNBytesInRow) {
      newRowStarted := true.B
      regAgentRowCnt := regAgentRowCnt+1.U
    }.otherwise {
      newRowStarted := false.B
    }

    when(regBytesLeftInRow > (64/p.bitWidth).U) {
      regBytesLeftInRow := regBytesLeftInRow - (64/p.bitWidth).U // Doenst matter if it overflows the last round
    }.otherwise {
      regBytesLeftInRow := regNBytesInRow
      regNRowsReceived := regNRowsReceived + 1.U
    }
    bramEls
  }

  val sIdle :: sRunning :: Nil = Enum(2)
  val regState = RegInit(sIdle)
  val regBramLine = RegInit(0.U.asTypeOf(new BramLine(p)))
  val regBramElsLeftOvers = RegInit(VecInit(Seq.fill(64/p.bitWidth)(0.U.asTypeOf(new BramEl(p)))))

  val regElCnt = RegInit(0.U(log2Ceil(p.nPEs).W))
  val regRowAddrCnt = RegInit(0.U(p.bramAddrWidth.W))
  val regColAddrCnt = RegInit(0.U(p.bramAddrWidth.W))
  val regAgentRowCnt = RegInit(0.U((p.agentWidth+1).W))
  val regBramRowCnt = RegInit(0.U(p.bramAddrWidth.W))

  val regNRows = RegInit(0.U((p.agentWidth.W)))
  val regNCols = RegInit(0.U((p.agentWidth.W)))
  val regNBytesInRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowsPerMatrixRow = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regDramRowIdx = RegInit(0.U(log2Ceil(p.maxProblemSize*8).W))
  val regNRowsReceived = RegInit(0.U(p.agentWidth.W)) // Count how many rows we have fully recevied from DRAM
  val newRowStarted = WireInit(true.B)



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
        val bytesInRow = (io.nCols >> ((p.bitWidth>>3)-1))
        regNBytesInRow := bytesInRow
        regBytesLeftInRow := bytesInRow

        regState := sRunning


        regNCols := io.nCols
        regNRows := io.nRows
        regNRowsReceived := 0.U
        regDramRowIdx := 0.U
        regElCnt := 0.U
        regBramLine := 0.U.asTypeOf(new BramLine(p))
        regAgentRowCnt := 0.U
      }
    }
    is (sRunning) {
      rspQ.deq.ready := true.B
      when (rspQ.deq.fire()) {
        val bramEls = dramRowToBramEls(rspQ.deq.bits.readData)
        bramEls.zipWithIndex.map{case (b,i) =>
          val col = WrapAdd(regColAddrCnt,i.U, regNCols)
          b.col := col
          b.last := col === (regNCols-1.U)
          printf("val=%d last=%d", b.value, b.last)
        }
        printf("\n")

        val nValids = PopCount(bramEls.map(_.value =/= 0.U))
        printf("nValids = %d\n", nValids)
        val valids = Compactor(bramEls, bramEls.map(_.value =/= 0.U))
        // Set the last valid
        valids(nValids-1.U).last := bramEls.map(_.last).reduce(_||_)
        valids.map( v =>
          printf("value: %d last:%d", v.value,v.last)
        )
        printf("\n")
        // First check whether we have any new rows starting this round
        when(newRowStarted) {
          when(nValids > 0.U) {
            io.agentRowAddress.valid := true.B
            io.agentRowAddress.bits.agentId := regAgentRowCnt
            io.agentRowAddress.bits.colAddr := regElCnt
            io.agentRowAddress.bits.rowAddr := regBramRowCnt
          }
        }

        val newRegElCnt = Cat(0.U(1.W), regElCnt) + nValids
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

          // Add the remaining to the regBramLine
          valids.zipWithIndex.map{case (el,i) =>
            when(i.U>=spotsLeft) {
              regBramLine.els(i.U-spotsLeft) := el
            }
          }
          // Increment the idx
          regElCnt := regElCnt + (p.nPEs.U - spotsLeft)

        }.otherwise{
          // Add the valid guys to the bramLine and increment
          valids.zipWithIndex.map{case (el, i) =>
            when(i.U < nValids) {
              regBramLine.els(regElCnt + i.U) := el
            }
          }
          // Increment the idx
          regElCnt := newRegElCnt
        }
      }.elsewhen(regNRowsReceived === regNRows) {
        // We dont get anymore lines since we got'em all. Send out the stored BRAM-line if a
        when(regElCnt > 0.U) {
          io.bramCmd.req.writeEn := true.B
          io.bramCmd.req.writeData := regBramLine.asUInt()
        }
        regState := sIdle
      }
    }
  }
}
