package core

import chisel3._
import chisel3.util._

class RobEntry extends Bundle {
  val completed = Bool()
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val writeRd = Bool()
  // 写入寄存器逻辑ID
  val rdLidx = UInt(5.W)
  // 写入寄存器物理ID
  val rdPidx = UInt(BackendConfig.pregIdxWidth)

  val jump = Bool()
  val jumpTarget = UInt(BusConfig.ADDR_WIDTH)

  val exception = Bool()
  val exceptionCode = UInt(InsConfig.EXECEPTION_WIDTH)
  
}

class NewRobRequest extends Bundle {
  val valid = Input(Bool())

  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  val writeRd = Input(Bool())
  val rdLidx = Input(UInt(5.W))
  val rdPidx = Input(UInt(BackendConfig.pregIdxWidth))
  val exception = Input(Bool())
  val exceptionCode = Input(UInt(InsConfig.EXECEPTION_WIDTH))


  val idx = Output(UInt(BackendConfig.robSize.W))
}

class RobNewIO extends Bundle {
  val news = Vec(FrontendConfig.decoderNum, new NewRobRequest)
  val newsCount = Input(UInt((log2Ceil(FrontendConfig.decoderNum)).W))

  val restSize = Output(UInt((log2Ceil(BackendConfig.robSize)).W))
}

class RobReadPcRequest extends Bundle {
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
}

class RobCompleteRequest extends Bundle {
  val valid = Output(Bool())
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val jump = Output(Bool())
  val jumpTarget = Output(UInt(BusConfig.ADDR_WIDTH))
}

class ReorderBuffer extends Module {
  val newIO = IO(new RobNewIO)
  val completeIO = IO(Vec(BackendConfig.pipelineNum, Flipped(new RobCompleteRequest)))

  val entries = RegInit(
    VecInit(Seq.fill(BackendConfig.robSize)(0.U.asTypeOf(new RobEntry)))
  )

  val head = RegInit(0.U(BackendConfig.robIdxWidth))
  val tail = RegInit(0.U(BackendConfig.robIdxWidth))

  val count = tail - head

  newIO.restSize := BackendConfig.robSize.U - count


  // 处理rename阶段发送的ROB新建请求

  for (i <- 0 until FrontendConfig.decoderNum) {
    val idx = tail + i.U
    when(newIO.news(i).valid) {
      val entry = Wire(new RobEntry)
      entry.completed := false.B

      entry.vaddr := newIO.news(i).vaddr
      entry.writeRd := newIO.news(i).writeRd
      entry.rdLidx := newIO.news(i).rdLidx
      entry.rdPidx := newIO.news(i).rdPidx
      entry.jump := false.B
      entry.jumpTarget := 0.U
      entry.exception := newIO.news(i).exception
      entry.exceptionCode := newIO.news(i).exceptionCode
      
      entries(idx) := entry
    }
    newIO.news(i).idx := idx
  }

  tail := tail + newIO.newsCount

  // 处理流水线写回的complete请求
  for (i <- 0 until BackendConfig.pipelineNum) {
    val complete = completeIO(i)
    when(complete.valid) {
      entries(complete.robIdx).completed := true.B
      entries(complete.robIdx).jump := complete.jump
      entries(complete.robIdx).jumpTarget := complete.jumpTarget
    }
  }



  assert(newIO.newsCount < newIO.restSize)
  assert(PopCount(newIO.news.map(_.valid)) === newIO.newsCount)
}
