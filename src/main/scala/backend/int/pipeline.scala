package core

import chisel3._
import chisel3.util._

class IntPipeline(index: Int) extends Module 
  with InstructionConstants {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new IntInstruction))

    val regRead = Vec(2, new RegisterReadRequest)
    val regWrite = new RegisterWriteRequest
    val pcRead = new RobReadPcRequest
    val robComplete = new RobCompleteRequest
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  BackendUtils.BroadcastWakeup(index, io.in.bits.prd, io.in.valid && io.in.bits.writeRd)
  io.in.ready := true.B

  val f0_ins = RegInit(0.U.asTypeOf(new IntInstruction))
  val f0_valid = RegInit(false.B)

  f0_ins := Mux(io.in.valid, io.in.bits, 0.U)
  f0_valid := io.in.valid

  // F1 读寄存器阶段
  val f1_src1 = Reg(UInt(32.W))
  val f1_src2 = Reg(UInt(32.W))
  val f1_ins = RegInit(0.U.asTypeOf(new IntInstruction))
  val f1_pc = RegInit(0.U(32.W))
  val f1_valid = RegInit(false.B)

  io.regRead(0).id := f0_ins.prs1
  io.regRead(1).id := f0_ins.prs2
  io.pcRead.robIdx := f0_ins.robIdx

  f1_src1 := io.regRead(0).value
  f1_src2 := io.regRead(1).value
  f1_ins := f0_ins
  f1_pc := io.pcRead.vaddr
  f1_valid := f0_valid

  // F2 执行阶段
  val f2_jump = RegInit(false.B)
  val f2_jumpTarget = RegInit(0.U(32.W))
  val f2_aluResult = RegInit(0.U(32.W))
  val f2_writeRd = RegInit(false.B)
  val f2_robIddx = Reg(UInt(BackendConfig.robIdxWidth))
  val f2_rd = Reg(UInt(BackendConfig.pregIdxWidth))
  val f2_valid = RegInit(false.B)
  

  val sideway = BackendUtils.SearchSideway(f1_ins.prs1, f1_ins.prs2)

  val src1 = MuxLookup(f1_ins.selOP1, 0.U)(
    Seq(
      OP1_RS1 -> Mux(sideway(0).valid, sideway(0).value, f1_src1),
      OP1_PC -> f1_pc
    )
  )
  val src2 = MuxLookup(f1_ins.selOP2, 0.U)(
    Seq(
      OP2_RS2 -> Mux(sideway(1).valid, sideway(1).value, f1_src2),
      OP2_IMM -> f1_ins.imm,
      OP2_FOUR -> 4.U
    )
  )

  val alu = Module(new ALU)
  val bru = Module(new BRU)

  alu.io.src1 := src1
  alu.io.src2 := src2
  alu.io.aluType := f1_ins.aluType

  bru.io.pc := f1_pc
  bru.io.rs1 := src1
  bru.io.rs2 := src2
  bru.io.bruType := f1_ins.bruType
  bru.io.imm := f1_ins.imm
  bru.io.aluOut := alu.io.out

  f2_jump := bru.io.doJump
  f2_jumpTarget := bru.io.jumpTarget
  f2_aluResult := alu.io.out
  f2_valid := f1_valid
  f2_writeRd := f1_ins.writeRd
  f2_robIddx := f1_ins.robIdx
  f2_rd := f1_ins.prd

  // F3 写回阶段
  
  val writeValid = f2_valid && f2_writeRd
  BackendUtils.BroadcastSideway(index, f2_rd, f2_aluResult, writeValid)

  io.regWrite.valid := writeValid
  io.regWrite.id := f2_rd
  io.regWrite.value := f2_aluResult

  
  io.robComplete.valid := f2_valid
  io.robComplete.robIdx := f2_robIddx
  io.robComplete.jump := f2_jump
  io.robComplete.jumpTarget := f2_jumpTarget


  // Flush 逻辑
  
  when (ctrlIO.flush) {
    f0_valid := false.B
    f1_valid := false.B
    f2_valid := false.B
    io.regWrite.valid := false.B
    io.robComplete.valid := false.B
  }
}
