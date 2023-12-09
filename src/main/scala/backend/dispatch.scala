package core

import chisel3._
import chisel3.util._
import CsrConstants._

class DispatchUnit extends Module with InstructionConstants {
  val io = IO(new Bundle {
    val in = Vec(FrontendConfig.decoderNum, Input(new PipelineInstruction))
    val done = Output(Bool())

    // 整数流水线，每条流水线一个入队端口
    val ints = Vec(BackendConfig.intPipelineNum, Decoupled(new IntInstruction))
    // 访存流水线，不支持乱序访存，一条流水线多个入队端口
    val mem = Vec(FrontendConfig.decoderNum, Decoupled(new MemoryInstruction))

    val csr = Output(new CsrInstruction) // 连接ExceptionUnit

    val interruptInitializing = Input(Bool())
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val intAllocBegin = RegInit(0.U(log2Ceil(BackendConfig.intPipelineNum).W))

  val isInt = VecInit(io.in.map(x => x.valid && x.iqtType === IQT_INT)).asUInt
  val isMem = VecInit(io.in.map(x => x.valid && x.iqtType === IQT_MEM)).asUInt

  val intIssue = Wire(Vec(BackendConfig.intPipelineNum, Bool()))
  val memIssue = Wire(Vec(FrontendConfig.decoderNum, Bool()))

  val intSucc =
    (intIssue.asUInt & VecInit(io.ints.map(_.ready)).asUInt) === intIssue.asUInt
  val memSucc =
    (memIssue.asUInt & VecInit(io.mem.map(_.ready)).asUInt) === memIssue.asUInt

  val succ = intSucc && memSucc
  io.done := succ && !io.interruptInitializing

  intIssue.foreach(_ := false.B)
  memIssue.foreach(_ := false.B)
  io.ints.zip(intIssue).foreach {
    case (x, y) => {
      x := DontCare
      x.valid := y && succ
    }
  }
  io.mem.zip(memIssue).foreach {
    case (x, y) => {
      x := DontCare
      x.valid := y && succ
    }
  }

  val csrInstructionBuffer = RegInit(0.U.asTypeOf(new CsrInstruction))
  io.csr := csrInstructionBuffer
  
  val isCsr = VecInit(io.in.map(x => x.valid && x.csrTag))

  when (isCsr.reduce(_ || _)) {
    if (DebugConfig.printDispatch) {
      DebugUtils.Print(cf"dispatch isCsr: ${isCsr}")
    }
    val firstCsrIdx = PriorityEncoder(isCsr.asUInt)
    if (DebugConfig.printDispatch) {
      DebugUtils.Print(cf"find a csr instruction at ${firstCsrIdx}, related isCsr.asUInt: ${isCsr.asUInt}")
    }
    when(csrInstructionBuffer.csrType === CSRNONE) {
      csrInstructionBuffer := io.in(firstCsrIdx).GetCsrInstruction()
    }
    if (DebugConfig.printDispatch) {
      DebugUtils.Print(cf"this csr instruction is:")
      DebugUtils.Print(cf"csraddr: ${io.in(firstCsrIdx).csrAddr}, csrtype: ${io.in(firstCsrIdx).csrType}, csruimm: ${io.in(firstCsrIdx).csrUimm}, csrWen: ${io.in(firstCsrIdx).writeCsrEn}, csrRen: ${io.in(firstCsrIdx).readCsrEn}, csrPrs: ${io.in(firstCsrIdx).prs1}, csrPrd: ${io.in(firstCsrIdx).prd}")
    } 
  }



  // if (DebugConfig.printDispatch) {
  //   for(i <- 0 until BackendConfig.intPipelineNum) {
  //     when(io.ints(i).valid) {
  //       DebugUtils.Print(cf"intpipeline${i} dispatched, robidx: ${io.ints(i).bits.robIdx}")
  //     }
  //   }
  //   for(i <- 0 until FrontendConfig.decoderNum) {
  //     when(io.mem(i).valid) {
  //       DebugUtils.Print(cf"mem dispatched, robidx: ${io.mem(i).bits.robIdx} type: ${io.mem(i).bits.memType} addr_preg: ${io.mem(i).bits.prs1} value_preg: ${io.mem(i).bits.prd_or_prs2}")
  //     }
  //   }
  //   // print isMem
  //   DebugUtils.Print(
  //     cf"dispatch isMem: ${isMem.asTypeOf(Vec(FrontendConfig.decoderNum, Bool()))} isInt: ${isInt.asTypeOf(Vec(FrontendConfig.decoderNum, Bool()))}"
  //   )
  // }

  // TODO : 根据队列剩余容量的Dispatch
  var restInt = isInt
  var lastAlloc = intAllocBegin
  for (i <- 0 until BackendConfig.intPipelineNum) {
    val inc = lastAlloc + 1.U
    val outIdx = Mux(inc === BackendConfig.intPipelineNum.U, 0.U, inc)
    
    val rest = restInt.orR
    val selIdx = PriorityEncoder(restInt)

    intIssue(outIdx) := rest
    io.ints(outIdx).bits := io.in(selIdx).GetIntInstruction()

    restInt = restInt & (~UIntToOH(selIdx))
    lastAlloc = outIdx
  }

  var restMem = isMem
  for (i <- 0 until FrontendConfig.decoderNum) {
    val rest = restMem.orR
    val selIdx = PriorityEncoder(restMem)

    memIssue(i) := rest
    io.mem(i).bits := io.in(selIdx).GetMemoryInstruction()

    restMem = restMem & (~UIntToOH(selIdx))
  }

  when(succ) {
    intAllocBegin := UIntUtils.AddMod(BackendConfig.intPipelineNum)(intAllocBegin, PopCount(isInt))
  }

  require(BackendConfig.intPipelineNum >= FrontendConfig.decoderNum)
  
  when(ctrlIO.flush) {
    // intAllocBegin := 0.U // 我不知道这里要不要清零；黄先生之前没写，应该是不需要的。
    csrInstructionBuffer := 0.U.asTypeOf(new CsrInstruction)
  }
}
