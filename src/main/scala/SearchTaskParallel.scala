package auction

import chisel3._
import chisel3.util._

class SearchTaskResultPar(private val ap: AuctionParams) extends Bundle {
  val winner = UInt(ap.agentWidth.W)
  val bid = UInt(ap.bitWidth.W)
}



class SearchTaskParIO(ap: AuctionParams) extends Bundle {
  val benefitIn = Vec(ap.nPEs, Flipped(Decoupled(UInt(ap.bitWidth.W))))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.map(_.ready:= false.B)
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
}

// CompReg is the pipeline registers feeding into and out of the comparators
class CompReg(ap: AuctionParams) extends Bundle {
  val benefit = UInt(ap.bitWidth.W)
  val id = UInt(ap.agentWidth.W)
  val valid = Bool()

  override def cloneType = { new CompReg(ap).asInstanceOf[this.type] }
}

// WinnerReg is the registers for the 2 winner candidates
class WinnerReg(ap: AuctionParams) extends Bundle {
  val bid = UInt((ap.bitWidth+1).W)
  val id = UInt(ap.agentWidth.W)
  val valid = Bool()

  override def cloneType = { new WinnerReg(ap).asInstanceOf[this.type] }
}

class SearchTaskPar(ap: AuctionParams) extends MultiIOModule {
  require(ap.nPEs > 2)

  val io = IO(new SearchTaskParIO(ap))

  // We need a comparator tree that matches the number of PEs
  val compTreeDepth = log2Ceil(ap.nPEs)
  val compTreeNodesAtLevel = Seq.tabulate(compTreeDepth)(l => {
    ap.nPEs / (1 << l)
  })

  println(s"treeDepth = ${compTreeDepth}")
  for (i <- 0 until compTreeDepth) {
    println(compTreeNodesAtLevel(i))
  }
  // The we need the pipeline registers for the comparators
  // CompRegs should probably contain the benefit + the ID of the proposer. How do we do that.
  // We make a bundle for it?
  // Initialize like: RegInit(0.U.asTypeOf(new myBundle(ap))
  val compRegs = MixedVecInit(Seq.tabulate(compTreeDepth)(
    l => VecInit(Seq.fill(compTreeNodesAtLevel(l))(
      RegInit(0.U.asTypeOf(new CompReg(ap)))
    ))))

  // To save a CC we calculate both winner candidates
  val winnerCandidates = VecInit(Seq.fill(2)(RegInit(0.U.asTypeOf(new WinnerReg(ap)))))

  def get_parent_indices(idx: Int): (Int, Int) = {
    (idx/2, (idx/2)+1)
  }
  // Then we need the comparators.
  for(i <- 1 until compTreeDepth) {
    for (j <- 0 until compTreeNodesAtLevel(i)) {
      val (lhs, rhs) = get_parent_indices(j)
      compRegs(i)(j) := Mux(compRegs(i-1)(lhs).benefit >= compRegs(i-1)(rhs).benefit, compRegs(i-1)(lhs), compRegs(i-1)(rhs))
    }
  }
  // Calculate the 2 candidates
  winnerCandidates(0).id := compRegs(compTreeDepth-1)(0).id
  winnerCandidates(1).id := compRegs(compTreeDepth-1)(1).id
  winnerCandidates(0).valid := compRegs(compTreeDepth-1)(0).valid
  winnerCandidates(1).valid := compRegs(compTreeDepth-1)(1).valid
  winnerCandidates(0).bid := compRegs(compTreeDepth-1)(0).benefit - compRegs(compTreeDepth-1)(1).benefit
  winnerCandidates(1).bid := compRegs(compTreeDepth-1)(1).benefit - compRegs(compTreeDepth-1)(0).benefit


  // Then we need the state-machine handling the I/O
  val sIdle :: sProcess :: sFinished :: Nil = Enum(3)
  val regState = RegInit(sIdle)
  val regCompCount = RegInit(0.U(log2Ceil(compTreeDepth)))


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
        for (i <- 0 until ap.nPEs) {
          when(io.benefitIn(i).fire()) {
            compRegs(0)(i).benefit := io.benefitIn(i).bits
            compRegs(0)(i).valid := true.B
          } otherwise {
            compRegs(0)(i).valid := false.B
          }
        }
        regState := sProcess
        regCompCount := 0.U
      }
    }
    is (sProcess) {
      // For now, no pipelining. So disable new inputs
      io.benefitIn.map(_.ready := false.B)
      when (regCompCount === (compTreeDepth - 1).U) {
        regState := sFinished
      } otherwise {
        regCompCount := regCompCount + 1.U
      }
    }

    is (sFinished) {
      io.resultOut.valid := true.B

      val winnerBid = WireInit(0.U(ap.bitWidth))
      val winnerIdx = WireInit(0.U(ap.agentWidth))

      // Publish the winner. We must consider some corner cases
      // 1. What if the two candidates are equal? Then pick the first and make bid = 1
      //  1a. If both are 0 then we shouldnt make a bid
      //  1b. If both are >0 then we let 0 bid 1
      when(winnerCandidates(0).bid === 0.U) {
        assert(winnerCandidates(1).bid === 0.U)
        when(winnerCandidates(0).valid === true.B) {
          winnerBid := 1.U
          winnerIdx := winnerCandidates(0).id
        }
      }.elsewhen(winnerCandidates(0).bid(ap.bitWidth) === 0.U) {
        // 2. 0 has 0 as signed bit => its the winner
        assert(winnerCandidates(1).bid(ap.bitWidth) === 1.U)
        winnerBid := winnerCandidates(0).bid(ap.bitWidth,0)
        winnerIdx := winnerCandidates(0).id
      }.otherwise {
        // 1 is winner
        assert(winnerCandidates(0).bid(ap.bitWidth) === 1.U)

        winnerBid := winnerCandidates(1).bid(ap.bitWidth,0)
        winnerIdx := winnerCandidates(1).id
      }

      // Send the results out
      io.resultOut.bits.bid := winnerBid
      io.resultOut.bits.winner := winnerIdx

      when (io.resultOut.fire()) {
        regState := sIdle
      }
    }
  }
}


