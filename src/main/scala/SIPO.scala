package auction

import chisel3.util._
import chisel3._


class SIPO[Tin <: UInt, Tout <: UInt](genIn: Tin, genOut: Tout) extends Module {
  val io = IO(new Bundle {
    val in = Input(Flipped(Decoupled(genIn.cloneType)))
    val out = Output(Decoupled(genOut.cloneType))
  })

  val wIn = genIn.getWidth
  val wOut = genOut.getWidth

  require( (wOut % wIn) == 0, s"DRAM to BRAM width has remainder. wIn=$wIn wOut=$wOut")

  val steps = wOut/wIn
  val regCount = RegInit(0.U(log2Ceil(steps).W))
  val regData = RegInit(0.U(wOut.W))


  // Forward ready signals
  io.in.ready := io.out.ready
  io.out.valid := false.B
  io.out.bits := regData

  when(io.in.fire()) {
    // Shift the data
    regData := (regData << wIn).asUInt | io.in.bits
    regCount := regCount + 1.U
  }

  when (regCount === steps.U) {
    io.out.valid := true.B
    when(io.out.fire() && io.in.fire()) {
      regCount := 1.U
    }.elsewhen(io.out.fire()) {
      regCount := 0.U
    }
  }
}

