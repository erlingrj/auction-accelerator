package auction

import chisel3._
import chisel3.util._
import fpgatidbits.synthutils.PrintableParam

class SearchTaskParams(
  val bitWidth: Int,
  val nPEs: Int,
  val maxProblemSize: Int
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

class SearchTaskResult(private val ap: SearchTaskParams) extends Bundle {
  val winner = UInt(ap.agentWidth.W)
  val bid = UInt(ap.bitWidth.W)
}

class PEResult(private val ap: SearchTaskParams) extends Bundle {
  val benefit = UInt(ap.bitWidth.W)
  val oldPrice = UInt(ap.bitWidth.W)
  val id = UInt(ap.agentWidth.W)
  val last = Bool()
}


class SearchTaskIO(ap: SearchTaskParams) extends Bundle {
  val benefitIn = Vec(ap.nPEs, Flipped(Decoupled(new PEResult(ap))))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.map(_.ready:= false.B)
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
  override def cloneType = { new SearchTaskIO(ap).asInstanceOf[this.type] }
}

// CompReg is the pipeline registers feeding into and out of the comparators
class CompReg(ap: SearchTaskParams) extends Bundle {
  val benefit = UInt((ap.bitWidth).W)
  val id = UInt(ap.agentWidth.W)
  val runningBid = UInt(ap.bitWidth.W)
  val oldPrice = UInt(ap.bitWidth.W)

  override def cloneType = { new CompReg(ap).asInstanceOf[this.type] }
}


class SearchTask(ap: SearchTaskParams) extends MultiIOModule {
  require(ap.nPEs > 2)

  val io = IO(new SearchTaskIO(ap))
  io.driveDefaults()

  def get_parent_indices(idx: Int): (Int, Int) = {
    (idx*2, (idx*2)+1)
  }

  // We need a comparator tree that matches the number of PEs
  val compTreeDepth = log2Ceil(ap.nPEs) + 1
  val compTreeNodesAtLevel = Seq.tabulate(compTreeDepth)(l => {
    ap.nPEs / (1 << l)
  })
  val s_valid = RegInit(VecInit(Seq.fill(compTreeDepth)(false.B)))
  val s_last = RegInit(VecInit(Seq.fill(compTreeDepth)(false.B)))

  // The we need the pipeline registers for the comparators
  // CompRegs should probably contain the benefit + the ID of the proposer. How do we do that.
  // We make a bundle for it?
  // Initialize like: RegInit(0.U.asTypeOf(new myBundle(ap))

  val compRegs = Seq.tabulate(compTreeDepth)(i => Seq.fill(compTreeNodesAtLevel(i))(
    RegInit(0.U.asTypeOf(new CompReg(ap)))))

  val slast_runningWinner = RegInit(0.U.asTypeOf(new CompReg(ap)))
  val slast_valid = RegInit(false.B)
  val slast_last = RegInit(false.B)

  val stall = !io.resultOut.ready && slast_valid


  when (!stall) {
    io.benefitIn.map(_.ready := true.B)
    val fire = io.benefitIn.map(_.fire()).reduce((l,r) => l || r)
    s_valid(0) := fire

    when(fire) {
      // Do input
      s_last(0) := io.benefitIn(0).bits.last
      for (i <- 0 until ap.nPEs) {
        compRegs(0)(i).oldPrice := io.benefitIn(i).bits.oldPrice
        compRegs(0)(i).id := io.benefitIn(i).bits.id
        compRegs(0)(i).runningBid := ~0.U(ap.bitWidth.W) // Set to MAX value
        when(io.benefitIn(i).fire()) {
          compRegs(0)(i).benefit := io.benefitIn(i).bits.benefit
        } otherwise {
          compRegs(0)(i).benefit := 0.U
        }
      }
    }

      // Move the comparator regs forward
      for (i <- 1 until compTreeDepth) {
        s_valid(i) := s_valid(i-1)
        s_last(i) := s_last(i-1)
        for (j <- 0 until compTreeNodesAtLevel(i)) {
          val (lhs, rhs) = get_parent_indices(j)
          val parentLeft = compRegs(i - 1)(lhs)
          val parentRight = compRegs(i - 1)(rhs)

          when(parentLeft.benefit >= parentRight.benefit) {
            //printf(p"$i-$j lhs: $lhs won \n")
            compRegs(i)(j).id := parentLeft.id
            compRegs(i)(j).benefit := parentLeft.benefit
            compRegs(i)(j).oldPrice := parentLeft.oldPrice
            val hypoBid = parentLeft.benefit - parentRight.benefit
            compRegs(i)(j).runningBid := Mux(hypoBid < parentLeft.runningBid, hypoBid, parentLeft.runningBid)
          }.otherwise {
            //printf(p"$i-$j lhs: $rhs won \n")
            compRegs(i)(j).id := parentRight.id
            compRegs(i)(j).benefit := parentRight.benefit
            compRegs(i)(j).oldPrice := parentRight.oldPrice
            val hypoBid = parentRight.benefit - parentLeft.benefit
            compRegs(i)(j).runningBid := Mux(hypoBid < parentRight.runningBid, hypoBid, parentRight.runningBid)
          }
        }
      }
      // Move last forward
      val currentWinner = compRegs(compTreeDepth-1)(0)

      when (slast_last || !slast_valid) {
        slast_runningWinner := currentWinner
      }.otherwise {
        when(currentWinner.benefit > slast_runningWinner.benefit) {
          slast_runningWinner.id := currentWinner.id
          slast_runningWinner.benefit := currentWinner.benefit
          slast_runningWinner.oldPrice := currentWinner.oldPrice
          val hypoBid = currentWinner.benefit - slast_runningWinner.benefit
          slast_runningWinner.runningBid := Mux(hypoBid < currentWinner.runningBid, hypoBid, currentWinner.runningBid)
        }.otherwise {
          val hypoBid = slast_runningWinner.benefit - currentWinner.benefit
          slast_runningWinner.runningBid := Mux(hypoBid < slast_runningWinner.runningBid, hypoBid, slast_runningWinner.runningBid)

        }
      }
      slast_valid := s_valid.last
      slast_last := s_last.last
    }

    // Do output
    io.resultOut.valid := slast_valid && slast_last
  io.resultOut.bits.winner := slast_runningWinner.id
  when (slast_runningWinner.runningBid === 0.U && slast_runningWinner.benefit > 0.U) {
    io.resultOut.bits.bid := slast_runningWinner.oldPrice + 1.U
  }.elsewhen(slast_runningWinner.benefit === 0.U) {
    io.resultOut.bits.bid := 0.U
  }.otherwise {
    io.resultOut.bits.bid := slast_runningWinner.runningBid + slast_runningWinner.oldPrice
  }


  }


