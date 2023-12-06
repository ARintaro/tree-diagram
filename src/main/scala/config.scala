package core

import chisel3._
import chisel3.util.log2Ceil

object GenConfig {
  var verilator = false
  var innerUartModel = false

}

object DebugConfig {
  var debug = false
  // 1520984
  var printBegin = 50000.U
  var printEnd = 100000.U

  var printRenameTable = false
  var printRenameUnitIn = false
  
  var printRenameAlloc = false
  var printRenameFree = false

  var printRenameNew = false
  var printDispatch = false
  var printWriteBack = false
  var printIssue = false
  var printPreg = false
  var printLreg = false
  var printFetch = false
  var printRedirect = false
  var printFlush = false
  var printBusError = true

  val printRob = false
  val printRobNew = false
  val printPipeIns = false
  val printStoreBuffer = false

  val printException = false

  var printBusy = false
}

object SramConfig {
  val ADDR_WIDTH = 20.W
  val DATA_WIDTH = 32.W
  val DATA_BYTES_NUM = (DATA_WIDTH.get / 8).W
}

object BusConfig {
  val DATA_WIDTH = 32.W
  val DATA_BYTES_NUM = (DATA_WIDTH.get / 8).W
  val ADDR_WIDTH = 32.W

  val BASE_RAM_START = 0x80000000L
  val BASE_RAM_MASK = 0xFFC00000L
  val EXT_RAM_START = 0x80400000L
  val EXT_RAM_MASK = 0xFFC00000L
  val UART_START = 0x10000000L
  val UART_MASK = 0xFFFF0000L
}

object InsConfig {
  val INS_WIDTH = 32.W

  val EXCEPTION_WIDTH = 4.W
  object ExceptionCode {
    // Instruction Exceptions
    val EC_IA_MISALIGNED     = "b0000".U // 指令地址对齐错误
    val EC_IA_FAULT          = "b0001".U // 指令访问错误
    val EC_ILLEGAL           = "b0010".U // 非法指令
    val EC_BREAKPOINT        = "b0011".U // 断点

    // Load/Store Exceptions
    val EC_LA_MISALIGNED     = "b0100".U // 加载地址对齐错误
    val EC_LA_FAULT          = "b0101".U // 加载访问错误
    val EC_SAIA_MISALIGNED   = "b0110".U // 存储/原子操作地址对齐错误
    val EC_SAIA_FAULT        = "b0111".U // 存储/原子操作访问错误

    // Environment Call Exceptions
    val EC_U_ENV_CALL        = "b1000".U // 从用户态调用环境调用指令 - User Environment Call
    val EC_S_ENV_CALL        = "b1001".U // 从监管态调用环境调用指令 - Supervisor Environment Call
    val EC_M_ENV_CALL        = "b1011".U // 从机器态调用环境调用指令 - Machine Environment Call

    // Page Fault Exceptions
    val EC_IA_PAGE_FAULT     = "b1100".U // 指令页错误
    val EC_LA_PAGE_FAULT     = "b1101".U // 加载页错误
    val EC_SAIA_PAGE_FAULT   = "b1111".U // 存储/原子操作页错误

    // Software Interrupts
    val IT_U_SOFT_INT        = "b0000".U // 用户软件中断
    val IT_S_SOFT_INT        = "b0001".U // 监管软件中断
    val IT_M_SOFT_INT        = "b0011".U // 机器软件中断

    // Timer Interrupts
    val IT_U_TIMER_INT       = "b0100".U // 用户定时器中断
    val IT_S_TIMER_INT       = "b0101".U // 监管定时器中断
    val IT_M_TIMER_INT       = "b0111".U // 机器定时器中断

    // External Interrupts
    val IT_U_EXT_INT         = "b1000".U // 用户外部中断
    val IT_S_EXT_INT         = "b1001".U // 监管外部中断
    val IT_M_EXT_INT         = "b1011".U // 机器外部中断
  }


  object Opcode {
    val jal = "b1101111".U
    val jalr = "b1100111".U
    val branch = "b1100011".U
  }
}

object CacheConfig {
  val icache = new IcacheConfig(2, 4, 256)
}

object FrontendConfig {
  val fetchQueueSize = 16

  val decoderNum = 3
}

object BackendConfig {
  val physicalRegNum = 48
  val robSize = 8

  val pregIdxWidth = log2Ceil(physicalRegNum).W
  val robIdxWidth = log2Ceil(robSize).W

  val maxCommitsNum = 3 
  
  val intPipelineNum = 3
  val intQueueSize = 8
  val intQueueScanWidth = 4

  val memPipelineNum = 1
  val memQueueSize = 8
 
  val pipelineNum = intPipelineNum + memPipelineNum
  val wakeUpNum = pipelineNum
  val sidewayNum = pipelineNum
  // 唤醒和旁路网络中，intPipe占用[0, intPipelineNum - 1]
  // memPipe占用intPipelineNum

  val storeBufferSize = 8
  val storeBufferIdxWidth = log2Ceil(storeBufferSize).W
}