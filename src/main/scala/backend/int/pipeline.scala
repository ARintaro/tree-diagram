package core


import chisel3._
import chisel3.util._


class IntPipeline(index : Int) extends Module {
  val io = IO(new Bundle {
	val in = Flipped(Decoupled(new IntInstruction))
  })
  
  
  
}