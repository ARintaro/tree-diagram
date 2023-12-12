package core

import chisel3._
import chisel3.util._


class ALU extends Module with InstructionConstants {
    val io = IO(new Bundle {
        val src1 = Input(UInt(32.W))
        val src2 = Input(UInt(32.W))
        val aluType = Input(UInt(ALU_WIDTH))
        val out = Output(UInt(32.W))
    })

    io.out := MuxLookup(io.aluType, 0.U)(Seq(
        ALU_ADD -> (io.src1 + io.src2),
        ALU_SUB -> (io.src1 - io.src2),
        ALU_SLL -> (io.src1 << io.src2(4, 0)),
        ALU_SRL -> (io.src1 >> io.src2(4, 0)),
        ALU_SRA -> (io.src1.asSInt >> io.src2(4, 0)).asUInt,
        ALU_AND -> (io.src1 & io.src2),
        ALU_OR  -> (io.src1 | io.src2),
        ALU_XOR -> (io.src1 ^ io.src2),
        ALU_SLT -> (io.src1.asSInt < io.src2.asSInt).asUInt,
        ALU_SLTU-> (io.src1 < io.src2).asUInt,
        ALU_ANDN-> (io.src1 & (~io.src2)),
        ALU_MINU-> Mux(io.src1 < io.src2, io.src1, io.src2),
        ALU_XNOR-> (io.src1 ^ (~io.src2))
    ))
}


class BRU extends Module with InstructionConstants {
    val io = IO(new Bundle {
        val pc = Input(UInt(32.W))
        val rs1 = Input(UInt(32.W))
        val rs2 = Input(UInt(32.W))
        val imm = Input(UInt(32.W))
        val bruType = Input(UInt(BRU_WIDTH))

        val aluOut = Input(UInt(32.W)) // B型指令的跳转地址是ALU计算的结果

        val doJump = Output(Bool())   // 是否跳转
        val jumpTarget = Output(UInt(32.W))  // 跳转目标地址
    })

    val eqSignal   = io.rs1 === io.rs2
    val ltSignal   = io.rs1.asSInt < io.rs2.asSInt
    val ltuSignal  = io.rs1 < io.rs2

    io.doJump := MuxLookup(io.bruType, false.B)(Seq(
        BRU_NONE -> false.B,
        BRU_EQ   -> eqSignal,
        BRU_NE   -> !eqSignal,
        BRU_LT   -> ltSignal,
        BRU_GE   -> !ltSignal,
        BRU_LTU  -> ltuSignal,
        BRU_GEU  -> !ltuSignal,
        BRU_JALR -> true.B
    ))

    io.jumpTarget := Mux(io.bruType === BRU_JALR, io.rs1 + io.imm, io.aluOut)

}