package auction

import chisel3._
import chisel3.util._

// WrapAdd adds v+add and wraps around at max.
//  adopted from RISCV BOOM core
object WrapAdd {
  def apply(v: UInt, add: UInt, max: Int): UInt = {
    if (isPow2(max)) {
      (v+add)(log2Ceil(max)-1,0)
    } else {
      val sum = Cat(0.U(1.W),v) + Cat(0.U(1.W), add)
      Mux(sum >= max.U,
        sum - max.U,
        sum)
    }
  }
  def apply(v: UInt, add: UInt, max: UInt): UInt = {
      val sum = Cat(0.U(1.W),v) + Cat(0.U(1.W), add)
      Mux(sum >= max,
        sum - max,
        sum)
  }
}


// Takes input vector and a valids vector and outputs a compacted vector with the valids shifted down
object Compactor {
  def apply[T <: Data](vIn: Vec[T], valids: Seq[Bool]): Vec[T] = {
    val vOut = WireInit(VecInit(Seq.fill(vIn.length)(0.U.asTypeOf(vIn(0)))))
    val idxs = WireInit(VecInit(Seq.fill(vIn.length)(0.U(log2Ceil(vIn.length).W))))

    for (i <- 0 until vIn.length) {
      if (i == 0) {
        when(valids(0)) {
          vOut(0) := vIn(0)
          idxs(0) := 1.U
        }.otherwise{
          idxs(0) := 0.U
        }
      } else {
        when (valids(i)) {
          vOut(idxs(i-1)) := vIn(i)
          idxs(i) := idxs(i-1) + 1.U
        }.otherwise {
          idxs(i) := idxs(i-1)
        }
      }

    }

    vOut
  }
}