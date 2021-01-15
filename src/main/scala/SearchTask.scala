package auction

import chisel3._
import chisel3.util._


// The search tasks takes in one net benefit at the time and calculates the
// total highest, its index and its bid which is passed along to next node
class SearchTaskResult(private val ap: AuctionParams) extends Bundle {
  val winner = UInt(log2Ceil(ap.nPEs).W)
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

class SearchTaskPar(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new SearchTaskParIO(ap))
  val regCurrentBest = RegInit(0.U(ap.bitWidth.W))
  val regCurrentNextBest = RegInit(0.U(ap.bitWidth.W))
  val regCount = RegInit(0.U(log2Ceil(ap.nPEs).W))
  val regCurrentBestIdx = RegInit(0.U(log2Ceil(ap.nPEs).W))


  // We need a comparator tree that matches the number of PEs
  val compTreeDepth = log2Ceil(ap.nPEs)
  val compTreeNodesAtLevel = Seq.tabulate(compTreeDepth)(l => ap.nPEs / (2^l))

  // The we need the pipeline registers for the comparators
  // CompRegs should probably contain the benefit + the ID of the proposer. How do we do that.
  // We make a bundle for it?
  // Initialize like: RegInit(0.U.asTypeOf(new myBundle(ap))
  val compRegs = MixedVecInit(Seq.tabulate(compTreeDepth)(
    l => VecInit(Seq.fill(compTreeNodesAtLevel(l))(
      RegInit(0.U(ap.bitWidth.W))
    ))))

  def get_parent_indices(idx: Int): (Int, Int) = {
    (idx/2, (idx/2)+1)
  }
  // Then we need the comparators.
  for(i <- 1 until compTreeDepth) {
    for (j <- 0 until compTreeNodesAtLevel(i)) {
      val (lhs, rhs) = get_parent_indices(j)
      compRegs(i)(j) := Mux(compRegs(i-1)(lhs) >= compRegs(i-1)(rhs), compRegs(i-1)(lhs), compRegs(i-1)(rhs))
    }
  }

  // Then we need the state-machine handling the I/O

  val sIdle :: sProcess :: sFinished :: Nil = Enum(2)
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
            compRegs(0)(i) := io.benefitIn(i).bits
          } otherwise {
            compRegs(0)(i) := 0.U
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
      io.resultOut.bits.winner := regCurrentBestIdx
      // If we have a tie (and its valid != 0) we make a unit raise
      // Not sure if this is desired? Well well

      when (regCurrentBest === regCurrentNextBest && regCurrentBest > 0.U) {
        io.resultOut.bits.bid := 1.U
      }.otherwise{
        io.resultOut.bits.bid := regCurrentBest - regCurrentNextBest
      }

      when (io.resultOut.fire) {
        regState := sProcess
        regCurrentBest := 0.U
        regCurrentNextBest := 0.U
      }
    }
  }
}



class SearchTaskIO(ap: AuctionParams) extends Bundle {
  val benefitIn = Flipped(Decoupled(UInt(ap.bitWidth.W)))
  val resultOut = Decoupled(new SearchTaskResult(ap))

  def driveDefaults(): Unit = {
    benefitIn.ready:= false.B
    resultOut.valid := false.B
    resultOut.bits.winner := 0.U
    resultOut.bits.bid := 0.U
  }
}
class SearchTask(ap: AuctionParams) extends MultiIOModule {
  val io = IO(new SearchTaskIO(ap))
  val regCurrentBest = RegInit(0.U(ap.bitWidth.W))
  val regCurrentNextBest = RegInit(0.U(ap.bitWidth.W))
  val regCount = RegInit(0.U(log2Ceil(ap.nPEs).W))
  val regCurrentBestIdx = RegInit(0.U(log2Ceil(ap.nPEs).W))

  val sProcess :: sFinished :: Nil = Enum(2)
  val regState = RegInit(sProcess)

  // Drive interface signals to default
  io.driveDefaults

  switch (regState) {
    is (sProcess) {
      io.benefitIn.ready := true.B
      io.resultOut.valid := false.B
      io.resultOut.bits := DontCare
      when (io.benefitIn.fire) {
        when(io.benefitIn.bits > regCurrentBest && io.benefitIn.bits(ap.bitWidth-1) === false.B) {
          regCurrentBest := io.benefitIn.bits
          regCurrentNextBest := regCurrentBest
          regCurrentBestIdx := regCount
        }
          .otherwise
          {
            when(io.benefitIn.bits > regCurrentNextBest && io.benefitIn.bits(ap.bitWidth-1) === false.B) {
              regCurrentNextBest := io.benefitIn.bits
            }
          }
        // Increment count
        when(regCount === (ap.nPEs - 1).U) {
          regCount := 0.U
          regState := sFinished
        }. otherwise {
          regCount := regCount + 1.U
        }
      }
    }
    is (sFinished) {
      io.benefitIn.ready := false.B
      io.resultOut.valid := true.B
      io.resultOut.bits.winner := regCurrentBestIdx
      // If we have a tie (and its valid != 0) we make a unit raise
      // Not sure if this is desired? Well well

      when (regCurrentBest === regCurrentNextBest && regCurrentBest > 0.U) {
        io.resultOut.bits.bid := 1.U
      }.otherwise{
        io.resultOut.bits.bid := regCurrentBest - regCurrentNextBest
      }

      when (io.resultOut.fire) {
        regState := sProcess
        regCurrentBest := 0.U
        regCurrentNextBest := 0.U
      }
    }
  }
}

