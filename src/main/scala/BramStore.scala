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

class BramStoreWriteReq(private val p: BramStoreParams) extends Bundle {
  val addr = UInt(p.agentWidth.W)
  val data = UInt(p.bitWidth.W)
}

class BramStoreIO(p: BramStoreParams) extends Bundle {
  val peReadReq = Vec(p.nPEs, Input(UInt(p.agentWidth.W)))
  val peReadRsp = Vec(p.nPEs, Output(UInt(p.bitWidth.W)))

  val assReadReq = Input(UInt(p.agentWidth.W))
  val assReadRsp = Output(UInt(p.bitWidth.W))

  val assWriteReq = Flipped(Decoupled(Input(new BramStoreWriteReq(p))))


  val dump = Input(Bool())
  val dumpOut = Decoupled(UInt(p.bitWidth.W))

}

class BramStore(p: BramStoreParams) extends MultiIOModule {

  val io = IO(new BramStoreIO(p))


  val mems = for (i <- 0 until p.nPEs+1) yield {
    Module(new SimpleDualPortBRAM(p.agentWidth, p.bitWidth))
  }

  val regReadReqs = RegInit(VecInit(Seq.fill(p.nPEs)(0.U(p.agentWidth.W))))

  // Default values at memory interface
  val wWriteEn = WireInit(false.B)
  val wWriteAddr = WireInit(0.U(p.agentWidth.W))
  val wWriteData = WireInit(0.U(p.bitWidth.W))
  mems.map(_.io.write.req.writeEn := wWriteEn)
  mems.map(_.io.write.req.addr := wWriteAddr)
  mems.map(_.io.write.req.writeData := wWriteData)

  mems.map(_.io.read.req.writeEn := false.B)
  mems.map(_.io.read.req.writeData := false.B)
  mems.map(_.io.read.req.addr := DontCare)

  io.peReadRsp.map(_ := 0.U)
  io.assReadRsp := 0.U
  io.assWriteReq.ready := false.B

  io.dumpOut.valid := false.B
  io.dumpOut.bits := DontCare
  val dumpCnt = RegInit(0.U(p.agentWidth.W))


  val sNormal :: sDump :: Nil = Enum(2)
  val regState = RegInit(sNormal)

  switch(regState) {
    is(sNormal) {

      // Do read request
        for (i <- 0 until p.nPEs) {
          mems(i).io.read.req.addr := io.peReadReq(i)
          regReadReqs(i) := io.peReadReq(i)
        }
    mems.last.io.read.req.addr := io.assReadReq

      // Do read response
      for (i <- 0 until p.nPEs) {
        when(wWriteEn && regReadReqs(i) === wWriteAddr) {
          // Forwarding
          io.peReadRsp(i) := wWriteData
        }.otherwise {
          io.peReadRsp(i) := mems(i).io.read.rsp.readData
        }
      }
      io.assReadRsp := mems.last.io.read.rsp.readData


      // Do writes from AssignmentEngine
      io.assWriteReq.ready := true.B
      when(io.assWriteReq.fire()) {
        wWriteEn := true.B
        wWriteAddr := io.assWriteReq.bits.addr
        wWriteData := io.assWriteReq.bits.data
      }


      when(io.dump) {
        dumpCnt := 0.U
        regState := sDump
        mems.last.io.read.req.addr := 0.U
      }


    }
    is(sDump) {

      io.assWriteReq.ready := false.B

      mems.last.io.read.req.addr := dumpCnt

      io.dumpOut.valid := true.B
      io.dumpOut.bits := mems.last.io.read.rsp.readData


      when(io.dumpOut.fire()) {

        when(dumpCnt < (p.maxProblemSize - 1).U) {
          dumpCnt := dumpCnt + 1.U
          mems.last.io.read.req.addr := dumpCnt + 1.U
          wWriteEn := true.B
          wWriteData := 0.U
          wWriteAddr := dumpCnt
        }
      }

      when (!io.dump) {
        regState := sNormal
      }
    }
  }

}
