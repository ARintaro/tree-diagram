package core

import chisel3._
import chisel3.util._

// RV32I指令集
object RV32IPattern {
  val luiPattern = BitPat("b?????????????????????????0110111")
  val auipcPattern = BitPat("b?????????????????????????0010111")
  val jalPattern = BitPat("b?????????????????????????1101111")
  val jalrPattern = BitPat("b?????????????????000?????1100111")
  val beqPattern = BitPat("b?????????????????000?????1100011")
  val bnePattern = BitPat("b?????????????????001?????1100011")
  val bltPattern = BitPat("b?????????????????100?????1100011")
  val bgePattern = BitPat("b?????????????????101?????1100011")
  val bltuPattern = BitPat("b?????????????????110?????1100011")
  val bgeuPattern = BitPat("b?????????????????111?????1100011")
  val lbPattern = BitPat("b?????????????????000?????0000011")
  val lhPattern = BitPat("b?????????????????001?????0000011")
  val lwPattern = BitPat("b?????????????????010?????0000011")
  val lbuPattern = BitPat("b?????????????????100?????0000011")
  val lhuPattern = BitPat("b?????????????????101?????0000011")
  val sbPattern = BitPat("b?????????????????000?????0100011")
  val shPattern = BitPat("b?????????????????001?????0100011")
  val swPattern = BitPat("b?????????????????010?????0100011")
  val addiPattern = BitPat("b?????????????????000?????0010011")
  val sltiPattern = BitPat("b?????????????????010?????0010011")
  val sltiuPattern = BitPat("b?????????????????011?????0010011")
  val xoriPattern = BitPat("b?????????????????100?????0010011")
  val oriPattern = BitPat("b?????????????????110?????0010011")
  val andiPattern = BitPat("b?????????????????111?????0010011")
  val slliPattern = BitPat("b0000000??????????001?????0010011")
  val srliPattern = BitPat("b0000000??????????101?????0010011")
  val sraiPattern = BitPat("b0100000??????????101?????0010011")
  val addPattern = BitPat("b0000000??????????000?????0110011")
  val subPattern = BitPat("b0100000??????????000?????0110011")
  val sllPattern = BitPat("b0000000??????????001?????0110011")
  val sltPattern = BitPat("b0000000??????????010?????0110011")
  val sltuPattern = BitPat("b0000000??????????011?????0110011")
  val xorPattern = BitPat("b0000000??????????100?????0110011")
  val srlPattern = BitPat("b0000000??????????101?????0110011")
  val sraPattern = BitPat("b0100000??????????101?????0110011")
  val orPattern = BitPat("b0000000??????????110?????0110011")
  val andPattern = BitPat("b0000000??????????111?????0110011")
  val fencePattern = BitPat("b?????????????????000?????0001111")
  val ecallPattern = BitPat("b00000000000000000000000001110011")
  val ebreakPattern = BitPat("b00000000000100000000000001110011")
}

// 取指单元拉取出的指令
class RawInstruction extends Bundle {
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)
  val exception = Bool()
  val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)
}

trait InstructionConstants {
  // 发射队列类型
  val IQT_WIDTH = 2.W
  // 基本整数发射队列
  def IQT_INT = 0.U(IQT_WIDTH)
  // 仿存发射队列
  def IQT_MEM = 1.U(IQT_WIDTH)
  // 乘除发射队列
  def IQT_MUL = 2.U(IQT_WIDTH)

  // 需要的额外能力
  val FU_WIDTH = 2.W
  // 要求能读TIMER
  def INT_FU_TIMER = 0.U(FU_WIDTH)
  // 要求能够读写CSR
  def INT_FU_CSR = 1.U(FU_WIDTH)
  // 要求能够读写ROB
  def INT_FU_ROB = 2.U(FU_WIDTH)

  // 操作数一选择
  val OP1_WIDTH = 2.W
  def OP1_RS1 = 0.U(OP1_WIDTH)
  def OP1_ZERO = 1.U(OP1_WIDTH)
  def OP1_PC = 2.U(OP1_WIDTH)

  // 操作数二选择
  val OP2_WIDTH = 2.W
  def OP2_RS2 = 0.U(OP2_WIDTH)
  def OP2_IMM = 1.U(OP2_WIDTH)
  def OP2_ZERO = 2.U(OP2_WIDTH)
  def OP2_FOUR = 3.U(OP2_WIDTH) // constant 4

  // 跳转指令编码
  val BRU_WIDTH = 3.W
  def BRU_NONE = 0.U(BRU_WIDTH)
  def BRU_EQ = 1.U(BRU_WIDTH)
  def BRU_NE = 2.U(BRU_WIDTH)
  def BRU_LT = 3.U(BRU_WIDTH)
  def BRU_GE = 4.U(BRU_WIDTH)
  def BRU_LTU = 5.U(BRU_WIDTH)
  def BRU_GEU = 6.U(BRU_WIDTH)
  def BRU_JALR = 7.U(BRU_WIDTH) // 对于jalr指令，BRU进行跳转地址计算，ALU进行PC+4计算

  // 立即数类型
  val IMMT_WIDTH = 3.W
  def IMMT_I = 0.U(IMMT_WIDTH)
  def IMMT_S = 1.U(IMMT_WIDTH)
  def IMMT_B = 2.U(IMMT_WIDTH)
  def IMMT_U = 3.U(IMMT_WIDTH)
  def IMMT_J = 4.U(IMMT_WIDTH)
  def IMMT_NONE = 5.U(IMMT_WIDTH) // R-type指令没有立即数

  // ALU运算类型
  val ALU_WIDTH = 4.W
  def ALU_ADD = 0.U(ALU_WIDTH)
  def ALU_SUB = 1.U(ALU_WIDTH)
  // 逻辑左移
  def ALU_SLL = 2.U(ALU_WIDTH)
  // 逻辑右移
  def ALU_SRL = 3.U(ALU_WIDTH)
  // 算术右移
  def ALU_SRA = 4.U(ALU_WIDTH)
  def ALU_AND = 5.U(ALU_WIDTH)
  def ALU_OR = 6.U(ALU_WIDTH)
  def ALU_XOR = 7.U(ALU_WIDTH)
  // 有符号小于
  def ALU_SLT = 8.U(ALU_WIDTH)
  // 无符号小于
  def ALU_SLTU = 9.U(ALU_WIDTH)

  // MEM访存长度
  val MEM_LEN_WIDTH = 2.W
  def MEM_WORD = 0.U(MEM_LEN_WIDTH)
  def MEM_HALF = 1.U(MEM_LEN_WIDTH)
  def MEM_BYTE = 2.U(MEM_LEN_WIDTH)

  // storeType
  val STORE_TYPE_WIDTH = 2.W
  def LOAD_RAM = 0.U(STORE_TYPE_WIDTH) // 不是store指令，也不是读取MMIO的load指令
  def STORE_RAM = 1.U(STORE_TYPE_WIDTH) // store指令，写入RAM
  def STORE_MMIO = 2.U(STORE_TYPE_WIDTH) // store指令，写入MMIO
  def LOAD_MMIO = 3.U(STORE_TYPE_WIDTH) // load指令，读取MMIO

  // CSR指令类型
  val CSR_WIDTH = 4.W
  def CSRRW = 0.U(CSR_WIDTH)
  def CSRRS = 1.U(CSR_WIDTH)
  def CSRRC = 2.U(CSR_WIDTH)
  def CSRRWI = 3.U(CSR_WIDTH)
  def CSRRSI = 4.U(CSR_WIDTH)
  def CSRRCI = 5.U(CSR_WIDTH)
  def MRET = 6.U(CSR_WIDTH)
  def SRET = 7.U(CSR_WIDTH)
  def ECALL = 8.U(CSR_WIDTH)
  def EBREAK = 9.U(CSR_WIDTH)
}

// 解码单元解码出的指令
class DecodedInstruction extends Bundle with InstructionConstants {
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
  val memLen = UInt(MEM_LEN_WIDTH)

  val exception = Bool()
  val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)

  val predictJump = Bool()
  val predictTarget = UInt(BusConfig.ADDR_WIDTH)

  // 等待流水线全部执行完毕后进入发射队列
  val unique = Bool()
  // 在提交时刷新流水线
  val flush = Bool()

  val extType = Bool()

  def imm = {
    MuxLookup(immType, 0.U)( 
      Seq(
        IMMT_I -> Cat(Fill(21, inst(31)), inst(30, 25), inst(24, 21), inst(20)),
        IMMT_S -> Cat(Fill(21, inst(31)), inst(30, 25), inst(11, 8), inst(7)),
        IMMT_B -> Cat(Fill(20, inst(31)), inst(7), inst(30, 25), inst(11, 8), 0.U),
        IMMT_U -> Cat(inst(31), inst(30, 20), inst(19, 12), Fill(12, 0.U)),
        IMMT_J -> Cat(inst(31), inst(19, 12), inst(20), inst(30, 25), inst(24, 21), 0.U)
      )
    )
  }

  def rs1 = inst(19, 15)
  def rs2 = inst(24, 20)
  def rd = inst(11, 7)
}

// 重命名后在流水线中传递的指令
class PipelineInstruction extends Bundle with InstructionConstants {

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
  val memLen = UInt(MEM_LEN_WIDTH)

  // 等待流水线全部执行完毕后进入发射队列
  val unique = Bool()
  // 在提交时刷新流水线
  val flush = Bool()

  val extType = Bool()

  if (DebugConfig.debug) {
    val debugInst = UInt(InsConfig.INS_WIDTH)
    val debugVaddr = UInt(BusConfig.ADDR_WIDTH)
  }

  def GetIntInstruction(): IntInstruction = {
    val inst = Wire(new IntInstruction)
    inst.imm := imm
    inst.selOP1 := selOP1
    inst.selOP2 := selOP2
    inst.writeRd := writeRd
    inst.bruType := bruType
    inst.aluType := aluType
    inst.robIdx := robIdx
    inst.prs1 := prs1
    inst.prs2 := prs2
    inst.prd := prd
    inst
  }

  def GetMemoryInstruction(): MemoryInstruction = {
    val inst = Wire(new MemoryInstruction)
    inst.robIdx := robIdx
    inst.prs1 := prs1
    inst.prd_or_prs2 := Mux(memType, prs2, prd) 
    inst.imm := imm
    inst.memType := memType
    inst.memLen := memLen
    inst.extType := extType
    inst
  }

}
