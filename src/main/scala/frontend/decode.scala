package core

import chisel3._
import chisel3.util._


class Decoder extends Module {
  val io = IO(new Bundle {
	val in = Flipped(Decoupled(new RawInstruction))

	val out = Decoupled(new DecodedInstruction)
  })
  
  io.in.ready := io.out.ready

  
}

