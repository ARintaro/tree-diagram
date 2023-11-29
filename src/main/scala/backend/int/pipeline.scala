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

  val wakeupValid = WireInit(io.in.valid && io.in.bits.writeRd)
  BackendUtils.BroadcastWakeup(index, io.in.bits.prd, wakeupValid)
  io.in.ready := true.B

  val f0_ins = RegInit(0.U.asTypeOf(new IntInstruction))
  val f0_valid = RegInit(false.B)

  f0_ins := Mux(io.in.valid, io.in.bits, 0.U.asTypeOf(new IntInstruction))
  f0_valid := io.in.valid

  // F1 读寄存器阶段
  val f1_src1 = Reg(UInt(32.W))
  val f1_src2 = Reg(UInt(32.W))
  val f1_ins = RegInit(0.U.asTypeOf(new IntInstruction))
  val f1_pc = RegInit(0.U(32.W))
  val f1_valid = RegInit(false.B)

  val f1_sideway = BackendUtils.SearchSideway(f0_ins.prs1, f0_ins.prs2)

  io.regRead(0).id := f0_ins.prs1
  io.regRead(1).id := f0_ins.prs2
  io.pcRead.robIdx := f0_ins.robIdx

  f1_src1 := Mux(f1_sideway(0).valid, f1_sideway(0).value, io.regRead(0).value)
  f1_src2 := Mux(f1_sideway(1).valid, f1_sideway(1).value, io.regRead(1).value)
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
  

  val f2_sideway = BackendUtils.SearchSideway(f1_ins.prs1, f1_ins.prs2)

  val reg1 = Mux(f2_sideway(0).valid, f2_sideway(0).value, f1_src1)
  val reg2 = Mux(f2_sideway(1).valid, f2_sideway(1).value, f1_src2)

  val src1 = MuxLookup(f1_ins.selOP1, 0.U)(
    Seq(
      OP1_RS1 -> reg1,
      OP1_PC -> f1_pc
    )
  )
  val src2 = MuxLookup(f1_ins.selOP2, 0.U)(
    Seq(
      OP2_RS2 -> reg2,
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
  bru.io.rs1 := reg1
  bru.io.rs2 := reg2
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
  io.robComplete.exception := false.B
  io.robComplete.exceptionCode := 0.U
  if(DebugConfig.printWriteBack) {
    when (io.regWrite.valid) {
      DebugUtils.Print(cf"intPipe${index} writeback, rd: ${f2_rd}, value: ${f2_aluResult}")
    }
    when(io.robComplete.valid) {
      DebugUtils.Print(cf"complete${index}, robidx: ${io.robComplete.robIdx}, target: 0x${f2_jumpTarget}%x")
    }
  }
  io.robComplete.storeBufferIdx := DontCare
  io.robComplete.storeType := NO_STORE

  // Flush 逻辑
  
  when (ctrlIO.flush) {
    f0_valid := false.B
    f1_valid := false.B
    f2_valid := false.B
    io.regWrite.valid := false.B
    io.robComplete.valid := false.B
    
    assert(!io.in.valid)
  }
}

