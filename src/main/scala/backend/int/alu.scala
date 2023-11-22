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
        ALU_SLTU-> (io.src1 < io.src2).asUInt
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

    val eq_signal   = io.rs1 === io.rs2
    val lt_signal   = io.rs1.asSInt < io.rs2.asSInt
    val ltu_signal  = io.rs1 < io.rs2

    val jumpSignals = ListLookup(bruType,
    List                (false.B     , 0.U            ),
    Array(             
        BRU_NONE -> List(false.B     , 0.U            ),
        BRU_EQ   -> List(eq_signal   , io.aluOut      ),
        BRU_NE   -> List(!eq_signal  , io.aluOut      ),
        BRU_LT   -> List(lt_signal   , io.aluOut      ),
        BRU_GE   -> List(!lt_signal  , io.aluOut      ),
        BRU_LTU  -> List(ltu_signal  , io.aluOut      ),
        BRU_GEU  -> List(!ltu_signal , io.aluOut      ),
        BRU_JALR -> List(true.B      , io.rs1 + io.imm)
    ))

    io.doJump := jumpSignals(0)
    io.jumpTarget := jumpSignals(1) + io.pc

}