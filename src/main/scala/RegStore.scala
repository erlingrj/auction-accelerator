package auction

import chisel3._
import chisel3.util._
import fpgatidbits.synthutils.PrintableParam

class RegStoreParams(
  val nReadPorts: Int,
  val nWritePorts: Int,
  val nReadWritePorts: Int = 0,
  val addrBits: Int,
  val pipe: Boolean = false
) extends PrintableParam {

  override def headersAsList(): List[String] = {
    List(
    )
  }

  override def contentAsList(): List[String] = {
    List(
    )
  }

  def depth: Int = (1 << addrBits)
}

class RegStoreReq[T <: Data](gen: T, p:RegStoreParams) extends Bundle {
  val wen = Bool()
  val addr = UInt(p.addrBits.W)
  val wdata = gen.cloneType

  override def cloneType: RegStoreReq.this.type = new RegStoreReq(gen,p).asInstanceOf[this.type]
}

class RegStoreRsp[T <: Data](gen: T, p:RegStoreParams) extends Bundle {
  val rdata = gen.cloneType
  override def cloneType: RegStoreRsp.this.type = new RegStoreRsp(gen,p).asInstanceOf[this.type]
}


class RegStoreTransaction[T <: Data](gen: T, p:RegStoreParams) extends Bundle {
  val req = Decoupled(new RegStoreReq(gen,p))
  val rsp = Flipped(Decoupled(new RegStoreRsp(gen,p)))

  override def cloneType: RegStoreTransaction.this.type = new RegStoreTransaction(gen,p).asInstanceOf[this.type]

  def write(data: T, addr: UInt) = {
    req.valid := true.B
    req.bits.wen := true.B
    req.bits.addr := addr
    req.bits.wdata := data
    assert(req.ready)
  }

  def read(addr: UInt): T = {
    req.valid := true.B
    req.bits.wen := false.B
    req.bits.addr := addr
    req.bits.wdata := 0.U
    rsp.ready := true.B

   assert(req.ready && rsp.valid)

    rsp.bits.rdata
  }
}

class RegStoreIO[T <: Data](gen: T, p: RegStoreParams) extends Bundle {
  val wPorts = Vec(p.nWritePorts, Flipped((new RegStoreTransaction(gen,p))))
  val rPorts = Vec(p.nReadPorts, Flipped((new RegStoreTransaction(gen,p))))
  val rwPorts = Vec(p.nReadWritePorts, Flipped(new RegStoreTransaction(gen,p)))

  require(p.pipe == false)

  def driveDefaults {
    wPorts.map(p =>
      {
        p.req.ready := false.B
        p.rsp.bits := DontCare
        p.rsp.valid := false.B
      }
    )
    rPorts.map(p => {
      p.req.ready := false.B
      p.rsp.bits := DontCare
      p.rsp.valid := false.B
    })
    rwPorts.map( p => {
      p.req.ready := false.B
      p.rsp.bits := DontCare
      p.rsp.valid := false.B
    })
  }

  override def cloneType: RegStoreIO.this.type = new RegStoreIO(gen,p).asInstanceOf[this.type]
}

class RegStore[T <: Data](val gen: T, val p: RegStoreParams) extends MultiIOModule {
  val io = IO(new RegStoreIO(gen, p))
  io.driveDefaults

  // Make storage
  val data = RegInit(VecInit(Seq.fill(p.depth)(0.U.asTypeOf(gen))))

  // Serve requests
  io.wPorts.foreach(wp => {
    wp.req.ready := true.B
    when(wp.req.fire()) {
      assert(wp.req.bits.wen) // Only support writes on writeport
      data(wp.req.bits.addr) := wp.req.bits.wdata
    }
  }
  )

  io.rPorts.foreach(rp => {
    rp.req.ready := true.B
    when (rp.req.fire()) {
      assert(rp.rsp.ready) // We cannot request data if we are not ready
      assert(!rp.req.bits.wen)
      val rdata = WireInit(data(rp.req.bits.addr))
      if (p.pipe) {
        io.wPorts.foreach(wp => {
          when (wp.req.fire() && wp.req.bits.addr === rp.req.bits.addr) {
            rdata := wp.req.bits.wdata
          }
        })
      }

      rp.rsp.valid := true.B
      rp.rsp.bits.rdata := rdata
    }
  })

  io.rwPorts.foreach(rwp => {
    rwp.req.ready := true.B
    when(rwp.req.fire()) {
      when(rwp.req.bits.wen) {
        data(rwp.req.bits.addr) := rwp.req.bits.wdata
      }.otherwise{
        val rdata = WireInit(data(rwp.req.bits.addr))
        if (p.pipe) {
          io.wPorts.foreach(wp => {
            when(wp.req.fire() && wp.req.bits.addr === rwp.req.bits.addr) {
              rdata := wp.req.bits.wdata
            }
          })
        }
        rwp.rsp.valid := true.B
        rwp.rsp.bits.rdata := rdata
      }
    }
  })

  when (reset.asBool()) {
    data := VecInit(Seq.fill(p.depth)(0.U.asTypeOf(gen)))
  }
}
