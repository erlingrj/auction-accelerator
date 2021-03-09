package auction

import chisel3._
import chisel3.util._
import fpgatidbits.dma.MemReqParams

// THe accountant keeps the books on assignments and prices and gives the prices to the PEs


abstract class Prices(val ap: AuctionParams) extends MultiIOModule {
  val io = IO(new PricesIO(ap))

  val regPrices = RegInit(VecInit(Seq.fill(ap.maxProblemSize)(0.U(ap.bitWidth.W))))
  io.driveDefaults()
}


class PriceReq(val ap: AuctionParams) extends Bundle {
  val objectId = UInt(ap.agentWidth.W)
}

class PriceResp(val ap: AuctionParams) extends Bundle {
  val price = UInt(ap.bitWidth.W)
}


class PriceUpdate(val ap: AuctionParams) extends Bundle {
  val objectId = UInt(ap.agentWidth.W)
  val price = UInt(ap.bitWidth.W)
}

class PricesIO(val ap: AuctionParams) extends Bundle {
  val peReqs = Vec(ap.nPEs, Flipped(Decoupled((new PriceReq(ap)))))
  val peResps = Vec(ap.nPEs, Decoupled(new PriceResp(ap)))

  val priceUpdate = Flipped(Decoupled(new PriceUpdate(ap)))
  val last = Input(Bool())

  def driveDefaults(): Unit = {
    priceUpdate.ready := false.B
    peReqs.map(_.ready := false.B)
    peResps.map(_.valid := false.B)
    peResps.map(_.bits := DontCare)
  }
}


// The no-fuzzy version. It doesnt allow price-lookup
class PricesNoFuzz(ap: AuctionParams)
  extends Prices(ap) {

  val sIdle :: sWaitForUpdate :: Nil = Enum(2)
  val regState = RegInit(sIdle)

  switch(regState) {
    is (sIdle) {
      io.peReqs.map(_.ready := true.B)
      when (io.peReqs(0).fire()) {
        io.peResps.zipWithIndex.map{case (p,i) =>
        p.valid := true.B
        p.bits.price := regPrices(io.peReqs(i).bits.objectId)}
        assert(io.peResps(0).ready === true.B)

        when (io.last) {
          regState := sWaitForUpdate
        }
      }
    }
    is (sWaitForUpdate) {
      io.priceUpdate.ready := true.B
      when (io.priceUpdate.fire()) {
        regPrices(io.priceUpdate.bits.objectId) := regPrices(io.priceUpdate.bits.objectId) + io.priceUpdate.bits.price
        regState := sIdle
      }
    }
  }


}