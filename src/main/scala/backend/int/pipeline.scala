package core


import chisel3._
import chisel3.util._


class IntPipeline(index : Int) extends Module {
  val io = IO(new Bundle {
	val in = Flipped(Decoupled(new IntInstruction))

	val regRead = Vec(2, new RegisterReadRequest)
  })

  BackendUtils.BroadcastWakeup(index, io.in.bits.prd, io.in.valid)
  io.in.ready := true.B

  val f0_ins = RegInit(0.U.asTypeOf(new IntInstruction))

  f0_ins := Mux(io.in.valid, io.in.bits, f0_ins)

  // 读寄存器阶段
  val f1_src1 = Reg(UInt(32.W))
  val f1_src2 = Reg(UInt(32.W))

  io.regRead(0).id := f0_ins.prs1
  io.regRead(1).id := f0_ins.prs2

  f1_src1 := io.regRead(0).value
  f1_src2 := io.regRead(1).value


  // 执行阶段

  // 执行阶段从旁路网络中获得数据


  // 写回阶段
  
  
}