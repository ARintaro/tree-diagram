package core

import chisel3._
import chisel3.util._

class Decoder extends Module {
  val io = IO(new Bundle {
    val in = Input(new RawInstruction)

    val out = Output(new DecodedInstruction)
  })


}

class DecodeUnit extends Module {
  val io = IO(new Bundle {
    // 连接到取指队列
    val in = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    val out = Vec(FrontendConfig.decoderNum, Decoupled(new DecodedInstruction))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val decoders = VecInit(Seq.fill(FrontendConfig.decoderNum)(Module(new Decoder).io))

  val result = Reg(Vec(FrontendConfig.decoderNum, new DecodedInstruction))
  val valids = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(false.B)))

  val empty = !valids.reduce(_ || _)
  for (i <- 0 until FrontendConfig.decoderNum) {
    // 如果自己是气泡，不管后面准没准备好都可以ready
    io.in(i).ready := io.out(i).ready || empty
    io.out(i).valid := valids(i)
    io.out(i).bits := result(i)
  }

  when (ctrlIO.flush) {
    valids.foreach(_ := false.B)

  } .otherwise {
    for (i <- 0 until FrontendConfig.decoderNum) {
      decoders(i).in := io.in(i).bits
      result(i) := decoders(i).out
      valids(i) := io.in(i).valid && decoders(i).out.valid
    }
  }

}