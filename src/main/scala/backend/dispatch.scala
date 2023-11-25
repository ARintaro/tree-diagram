package core

import chisel3._
import chisel3.util._

class DispatchUnit extends Module with InstructionConstants {
  val io = IO(new Bundle {
    val in = Vec(FrontendConfig.decoderNum, Input(new PipelineInstruction))
    val done = Output(Bool())

    // 整数流水线，每条流水线一个入队端口
    val ints = Vec(BackendConfig.intPipelineNum, Decoupled(new IntInstruction))
    // 访存流水线，不支持乱序访存，一条流水线多个入队端口
    val mem = Vec(FrontendConfig.decoderNum, Decoupled(new MemoryInstruction))
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
  io.done := succ

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

}
