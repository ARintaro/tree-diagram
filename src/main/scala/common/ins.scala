package core

import chisel3._
import chisel3.util._

// 取指单元拉取出的指令
class RawInstruction extends Bundle {
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)
}



trait InstructionConstants {
  // 发射队列类型
  val IQT_WIDTH = 2
  // 基本整数发射队列
  val IQT_INT = 0.U(IQT_WIDTH.W)
  // 仿存发射队列
  val IQT_MEM = 1.U(IQT_WIDTH.W)
  // 乘除发射队列
  val IQT_MUL = 2.U(IQT_WIDTH.W)

  // 需要的额外能力
  val FU_WIDTH = 2
  // 要求能读TIMER
  val INT_FU_TIMER = 0.U(FU_WIDTH.W)
  // 要求能够读写CSR
  val INT_FU_CSR = 1.U(FU_WIDTH.W)
  // 要求能够读写ROB
  val INT_FU_ROB = 2.U(FU_WIDTH.W)

  // 操作数一选择
  val OP1_WIDTH = 2.W
  val OP1_RS1 = 0.U(OP1_WIDTH) 
  val OP1_ZERO= 1.U(OP1_WIDTH) 
  val OP1_PC  = 2.U(OP1_WIDTH)

  // 操作数二选择
  val OP2_WIDTH = 2.W
  val OP2_RS2 = 0.U(OP2_WIDTH) 
  val OP2_IMM = 1.U(OP2_WIDTH) 
  val OP2_ZERO= 2.U(OP2_WIDTH) 
  val OP2_FOUR= 3.U(OP2_WIDTH) // constant 4

  // 跳转指令编码
  val BRU_WIDTH = 3.W
  val BRU_EQ = 0.U(BRU_WIDTH)
  val BRU_NE = 1.U(BRU_WIDTH)
  val BRU_LT = 2.U(BRU_WIDTH)
  val BRU_GE = 3.U(BRU_WIDTH)
  val BRU_LTU = 4.U(BRU_WIDTH)
  val BRU_GEU = 5.U(BRU_WIDTH)

  // 立即数类型
  val IMMT_WIDTH = 3.W
  val IMMT_I = 0.U(IMMT_WIDTH)
  val IMMT_S = 1.U(IMMT_WIDTH)
  val IMMT_B = 2.U(IMMT_WIDTH)
  val IMMT_U = 3.U(IMMT_WIDTH)
  val IMMT_J = 4.U(IMMT_WIDTH)

  // ALU运算类型
  val ALU_WIDTH = 4.W
  val ALU_ADD = 0.U(ALU_WIDTH)
  val ALU_SUB = 1.U(ALU_WIDTH)
  // 逻辑左移
  val ALU_SLL = 2.U(ALU_WIDTH)
  // 逻辑右移
  val ALU_SRL = 3.U(ALU_WIDTH)
  // 算术右移
  val ALU_SRA = 4.U(ALU_WIDTH)
  val ALU_AND = 5.U(ALU_WIDTH)
  val ALU_OR  = 6.U(ALU_WIDTH)
  val ALU_XOR = 7.U(ALU_WIDTH)
  // 有符号小于
  val ALU_SLT = 8.U(ALU_WIDTH)
  // 无符号小于
  val ALU_SLTU = 9.U(ALU_WIDTH)


}

// 解码单元解码出的指令
class DecodedInstruction extends Bundle
  with InstructionConstants {
  val valid = Bool()

  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)

  val immType = UInt(IMMT_WIDTH)

  // 操作数选择
  val selOP1 = UInt(OP1_WIDTH)
  val selOP2 = UInt(OP2_WIDTH)
  // 写入目的寄存器
  val writeRd = Bool()
  val bruType = UInt(BRU_WIDTH)
  val aluType = UInt(ALU_WIDTH)
  // 等待流水线全部执行完毕后进入发射队列
  val unique = Bool()
  // 在提交时刷新流水线
  val flush = Bool()


  def rs1 = inst(19, 15)
  def rs2 = inst(24, 20)
  def rd = inst(11, 7)
}

// 重命名后在流水线中传递的指令
class PipelineInstruction extends Bundle
  with InstructionConstants {

  // 在重排序缓存中的索引
  val robIdx = UInt(BackendConfig.robIdxWidth)
  val prs1 = UInt(BackendConfig.pregIdxWidth)
  val prs2 = UInt(BackendConfig.pregIdxWidth)
  val prd = UInt(BackendConfig.pregIdxWidth)

  val imm = UInt(32.W)

  // 操作数选择
  val selOP1 = UInt(OP1_WIDTH)
  val selOP2 = UInt(OP2_WIDTH)
  // 写入目的寄存器
  val writeRd = Bool()
  val bruType = UInt(BRU_WIDTH)
  val aluType = UInt(ALU_WIDTH)
  // 等待流水线全部执行完毕后进入发射队列
  val unique = Bool()
  // 在提交时刷新流水线
  val flush = Bool()

  if (DebugConfig.debug) {
    val debugInst = UInt(InsConfig.INS_WIDTH)
    val debugVaddr = UInt(BusConfig.ADDR_WIDTH)
  }
}


trait InstructionPattern {
  
}