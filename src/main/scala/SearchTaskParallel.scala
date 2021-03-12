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

class SearchTaskResultPar(private val ap: SearchTaskParams) extends Bundle {
  val winner = UInt(ap.agentWidth.W)
  val bid = UInt(ap.bitWidth.W)
}

class PEResult(private val ap: SearchTaskParams) extends Bundle {
  val benefit = UInt(ap.bitWidth.W)
  val id = UInt(ap.agentWidth.W)
  val last = Bool()
}


class SearchTaskParIO(ap: SearchTaskParams) extends Bundle {
  val benefitIn = Vec(ap.nPEs, Flipped(Decoupled(new PEResult(ap))))
  val resultOut = Decoupled(new SearchTaskResultPar(ap))

  def driveDefaults(): Unit = {
    benefitIn.map(_.ready:= false.B)
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
  override def cloneType = { new SearchTaskParIO(ap).asInstanceOf[this.type] }
}

// CompReg is the pipeline registers feeding into and out of the comparators
class CompReg(ap: SearchTaskParams) extends Bundle {
  val benefit = UInt((ap.bitWidth).W)
  val id = UInt(ap.agentWidth.W)
  val runningBid = UInt(ap.bitWidth.W)

  override def cloneType = { new CompReg(ap).asInstanceOf[this.type] }
}


class SearchTaskPar(ap: SearchTaskParams) extends MultiIOModule {
  require(ap.nPEs > 2)

  val io = IO(new SearchTaskParIO(ap))

  // We need a comparator tree that matches the number of PEs
  val compTreeDepth = log2Ceil(ap.nPEs) + 1
  val compTreeNodesAtLevel = Seq.tabulate(compTreeDepth)(l => {
    ap.nPEs / (1 << l)
  })

  //println(s"treeDepth = ${compTreeDepth}")
  for (i <- 0 until compTreeDepth) {
   // println(compTreeNodesAtLevel(i))
  }
  // The we need the pipeline registers for the comparators
  // CompRegs should probably contain the benefit + the ID of the proposer. How do we do that.
  // We make a bundle for it?
  // Initialize like: RegInit(0.U.asTypeOf(new myBundle(ap))

  val compRegs = Seq.tabulate(compTreeDepth)(i => Seq.fill(compTreeNodesAtLevel(i))(
    RegInit(0.U.asTypeOf(new CompReg(ap)))))

  val runningWinner = RegInit(0.U.asTypeOf(new CompReg(ap)))


  val regIsLast = RegInit(VecInit(Seq.fill(compTreeDepth)(false.B)))

  // Then we need the state-machine handling the I/O
  val sIdle :: sProcess :: sPostProcess :: sFinished :: Nil = Enum(4)
  val regState = RegInit(sIdle)
  val regCompCount = RegInit(0.U(log2Ceil(compTreeDepth).W))
  def get_parent_indices(idx: Int): (Int, Int) = {
    (idx*2, (idx*2)+1)
  }
  // Then we need the comparators.
  when(regState === sProcess) {
    for (i <- 1 until compTreeDepth) {
      for (j <- 0 until compTreeNodesAtLevel(i)) {
        val (lhs, rhs) = get_parent_indices(j)
        val parentLeft = compRegs(i - 1)(lhs)
        val parentRight = compRegs(i - 1)(rhs)

        when(parentLeft.benefit >= parentRight.benefit) {
          //printf(p"$i-$j lhs: $lhs won \n")
          compRegs(i)(j).id := parentLeft.id
          compRegs(i)(j).benefit := parentLeft.benefit
          val hypoBid = parentLeft.benefit - parentRight.benefit
          compRegs(i)(j).runningBid := Mux(hypoBid < parentLeft.runningBid, hypoBid, parentLeft.runningBid)
        }.otherwise {
          //printf(p"$i-$j lhs: $rhs won \n")
          compRegs(i)(j).id := parentRight.id
          compRegs(i)(j).benefit := parentRight.benefit
          val hypoBid = parentRight.benefit - parentLeft.benefit
          compRegs(i)(j).runningBid := Mux(hypoBid < parentRight.runningBid, hypoBid, parentRight.runningBid)
        }
      }
    }
  }
  // Pipe the tlast signal
  when(regState === sProcess) {
  for (i <- 1 until compTreeDepth) {
    regIsLast(i) := regIsLast(i-1)
  }
  }

  // Drive interface signals to default
  io.driveDefaults

  switch (regState) {
    is (sIdle) {
      // In idle state we are ready on all inputs
      io.benefitIn.map(_.ready := true.B)
      io.resultOut.valid := false.B
      io.resultOut.bits := DontCare

      // When we receive any data
      when(io.benefitIn.map(_.fire()).reduce((l,r) => l || r)) {
        regIsLast(0) := io.benefitIn(0).bits.last
        for (i <- 0 until ap.nPEs) {
          compRegs(0)(i).id := io.benefitIn(i).bits.id
          compRegs(0)(i).runningBid := ~0.U(ap.bitWidth.W) // Set to MAX value
          when(io.benefitIn(i).fire()) {
            compRegs(0)(i).benefit := io.benefitIn(i).bits.benefit
          } otherwise {
            compRegs(0)(i).benefit := 0.U
          }
        }
        regState := sProcess
        regCompCount := 0.U
      }
    }
    is (sProcess) {
      // For now, no pipelining. So disable new inputs
      io.benefitIn.map(_.ready := false.B)
      when (regCompCount === (compTreeDepth - 2).U) {
        regState := sPostProcess
      } otherwise {
        regCompCount := regCompCount + 1.U
      }
    }

    is (sPostProcess) {
      // In this stage we compare the winner of this round with the runningWinner from all rounds compared

      val currentWinner = compRegs(compTreeDepth-1)(0)

      when(currentWinner.benefit > runningWinner.benefit) {
        runningWinner.id := currentWinner.id
        runningWinner.benefit := currentWinner.benefit
        val hypoBid = currentWinner.benefit - runningWinner.benefit
        runningWinner.runningBid := Mux(hypoBid < currentWinner.runningBid, hypoBid, currentWinner.runningBid)
      }.otherwise {
        val hypoBid = runningWinner.benefit - currentWinner.benefit
        runningWinner.runningBid := Mux(hypoBid < runningWinner.runningBid, hypoBid, runningWinner.runningBid)
      }

      // Update runningWinner
      regState := sFinished

      }

    is (sFinished) {
      // Just check if we have a runningBid = 0 due to a tie, then we bid 1
      when (regIsLast(compTreeDepth-1) === true.B) {
        io.resultOut.valid := true.B
        io.resultOut.bits.winner := runningWinner.id
        when (runningWinner.runningBid === 0.U && runningWinner.benefit > 0.U) {
          io.resultOut.bits.bid := 1.U
        }.otherwise {
          io.resultOut.bits.bid := runningWinner.runningBid
        }

      }.otherwise{
        regState := sIdle
      }

      when(io.resultOut.fire()) {
        runningWinner.benefit := 0.U
        regState := sIdle
      }
      }
    }
  }


