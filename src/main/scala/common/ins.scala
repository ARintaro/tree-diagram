package core

import chisel3._
import chisel3.util._


// RV32I指令集
object RV32IPattern {
  val luiPattern    = BitPat("b?????????????????????????0110111")
  val auipcPattern  = BitPat("b?????????????????????????0010111")
  val jalPattern    = BitPat("b?????????????????????????1101111")
  val jalrPattern   = BitPat("b?????????????????000?????1100111")
  val beqPattern    = BitPat("b?????????????????000?????1100011")
  val bnePattern    = BitPat("b?????????????????001?????1100011")
  val bltPattern    = BitPat("b?????????????????100?????1100011")
  val bgePattern    = BitPat("b?????????????????101?????1100011")
  val bltuPattern   = BitPat("b?????????????????110?????1100011")
  val bgeuPattern   = BitPat("b?????????????????111?????1100011")
  val lbPattern     = BitPat("b?????????????????000?????0000011")
  val lhPattern     = BitPat("b?????????????????001?????0000011")
  val lwPattern     = BitPat("b?????????????????010?????0000011")
  val lbuPattern    = BitPat("b?????????????????100?????0000011")
  val lhuPattern    = BitPat("b?????????????????101?????0000011")
  val sbPattern     = BitPat("b?????????????????000?????0100011")
  val shPattern     = BitPat("b?????????????????001?????0100011")
  val swPattern     = BitPat("b?????????????????010?????0100011")
  val addiPattern   = BitPat("b?????????????????000?????0010011")
  val sltiPattern   = BitPat("b?????????????????010?????0010011")
  val sltiuPattern  = BitPat("b?????????????????011?????0010011")
  val xoriPattern   = BitPat("b?????????????????100?????0010011")
  val oriPattern    = BitPat("b?????????????????110?????0010011")
  val andiPattern   = BitPat("b?????????????????111?????0010011")
  val slliPattern   = BitPat("b0000000??????????001?????0010011")
  val srliPattern   = BitPat("b0000000??????????101?????0010011")
  val sraiPattern   = BitPat("b0100000??????????101?????0010011")
  val addPattern    = BitPat("b0000000??????????000?????0110011")
  val subPattern    = BitPat("b0100000??????????000?????0110011")
  val sllPattern    = BitPat("b0000000??????????001?????0110011")
  val sltPattern    = BitPat("b0000000??????????010?????0110011")
  val sltuPattern   = BitPat("b0000000??????????011?????0110011")
  val xorPattern    = BitPat("b0000000??????????100?????0110011")
  val srlPattern    = BitPat("b0000000??????????101?????0110011")
  val sraPattern    = BitPat("b0100000??????????101?????0110011")
  val orPattern     = BitPat("b0000000??????????110?????0110011")
  val andPattern    = BitPat("b0000000??????????111?????0110011")
  val fencePattern  = BitPat("b?????????????????000?????0001111")
  val ecallPattern  = BitPat("b00000000000000000000000001110011")
  val ebreakPattern = BitPat("b00000000000100000000000001110011")
}


// 取指单元拉取出的指令
class RawInstruction extends Bundle {
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)
}



trait InstructionConstants {
  // 发射队列类型
  val IQT_WIDTH = 2.W
  // 基本整数发射队列
  val IQT_INT = 0.U(IQT_WIDTH)
  // 仿存发射队列
  val IQT_MEM = 1.U(IQT_WIDTH)
  // 乘除发射队列
  val IQT_MUL = 2.U(IQT_WIDTH)

  // 需要的额外能力
  val FU_WIDTH = 2.W
  // 要求能读TIMER
  val INT_FU_TIMER = 0.U(FU_WIDTH)
  // 要求能够读写CSR
  val INT_FU_CSR = 1.U(FU_WIDTH)
  // 要求能够读写ROB
  val INT_FU_ROB = 2.U(FU_WIDTH)

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
  val BRU_NONE = 0.U(BRU_WIDTH)
  val BRU_EQ = 1.U(BRU_WIDTH)
  val BRU_NE = 2.U(BRU_WIDTH)
  val BRU_LT = 3.U(BRU_WIDTH)
  val BRU_GE = 4.U(BRU_WIDTH)
  val BRU_LTU = 5.U(BRU_WIDTH)
  val BRU_GEU = 6.U(BRU_WIDTH)
  val BRU_JALR = 7.U(BRU_WIDTH)  // 对于jalr指令，BRU进行跳转地址计算，ALU进行PC+4计算

  // 立即数类型
  val IMMT_WIDTH = 3.W
  val IMMT_I = 0.U(IMMT_WIDTH)
  val IMMT_S = 1.U(IMMT_WIDTH)
  val IMMT_B = 2.U(IMMT_WIDTH)
  val IMMT_U = 3.U(IMMT_WIDTH)
  val IMMT_J = 4.U(IMMT_WIDTH)
  val IMMT_NONE = 5.U(IMMT_WIDTH) // R-type指令没有立即数

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

  // MEM访存长度
  val MEM_LEN_TYPE = 2.W
  val MEM_WORD = 0.U(MEM_LEN_TYPE)
  val MEM_HALF = 1.U(MEM_LEN_TYPE)
  val MEM_BYTE = 2.U(MEM_LEN_TYPE)
}

// 解码单元解码出的指令
class DecodedInstruction extends Bundle
  with InstructionConstants {
  val valid = Bool()

  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)

  val immType = UInt(IMMT_WIDTH)
  val iqtType = UInt(IQT_WIDTH)

  // 操作数选择
  val selOP1 = UInt(OP1_WIDTH)
  val selOP2 = UInt(OP2_WIDTH)
  // 写入目的寄存器
  val writeRd = Bool()
  val bruType = UInt(BRU_WIDTH)
  val aluType = UInt(ALU_WIDTH)
  val memType = Bool() // true: store, false: load
  // 访存控制信号
  val memLenType = UInt(MEM_LEN_TYPE)


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
  val valid = Bool()
  val robIdx = UInt(BackendConfig.robIdxWidth)
  val iqtType = UInt(IQT_WIDTH)
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
  val memType = Bool() // true: store, false: load
  // 访存控制信号
  val memLen = UInt(MEM_LEN_TYPE)


  
  // 等待流水线全部执行完毕后进入发射队列
  val unique = Bool()
  // 在提交时刷新流水线
  val flush = Bool()

  if (DebugConfig.debug) {
    val debugInst = UInt(InsConfig.INS_WIDTH)
    val debugVaddr = UInt(BusConfig.ADDR_WIDTH)
  }

  
  def GetIntInstruction() : IntInstruction = {
    val inst = Wire(new IntInstruction)
    // TODO
    inst
  }

  def GetMemoryInstruction() : MemoryInstruction = {
    val inst = Wire(new MemoryInstruction)
    // TODO
    inst
  }
}


