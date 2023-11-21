package core

import chisel3._
import chisel3.util._

class Decoder extends Module {
  val io = IO(new Bundle {
    val in = Input(new RawInstruction)
    // 这条指令内的数据是否有效
    // 当valid为true，且指令内数据解码错误时，才能触发错误指令异常
    val valid = Input(Bool())

    val out = Output(new DecodedInstruction)
  })
  

}

class DecodeUnit extends Module {
  val io = IO(new Bundle {
    // 连接到取指队列
    val in = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    
    // Decode Unit 必须所有指令一起出去
    val out = Vec(FrontendConfig.decoderNum, Output(new DecodedInstruction))
    val outReady = Input(Bool())
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val decoders = VecInit(Seq.fill(FrontendConfig.decoderNum)(Module(new Decoder).io))

  val result = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(0.U.asTypeOf(new DecodedInstruction))))

  val empty = !result.map(_.valid).reduce(_ || _)
  val ready = io.outReady || empty
  
  for (i <- 0 until FrontendConfig.decoderNum) {
    // 如果自己是气泡，不管后面准没准备好都可以ready
    io.in(i).ready := ready
    decoders(i).in := io.in(i).bits
    decoders(i).valid := io.in(i).valid

    io.out(i) := result(i)
  }

  when (ctrlIO.flush) {
    result.foreach(_.valid := false.B)
    io.out.foreach(_.valid := false.B)

  } .elsewhen(ready) {
    for (i <- 0 until FrontendConfig.decoderNum) {
      result(i) := decoders(i).out
    }

  }

}