package core

import chisel3._
import chisel3.util._

class RegisterReadRequest extends Bundle {
  val id = Output(UInt(BackendConfig.pregIdxWidth))
  val value = Input(UInt(32.W))
}

class RegisterWriteRequest extends Bundle {
  val valid = Output(Bool())
  val id = Output(UInt(BackendConfig.pregIdxWidth))
  val value = Output(UInt(32.W))
}

class PhysicalRegisterFile(readPortNum: Int) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(readPortNum, Flipped(new RegisterReadRequest))
  })

  val regs = RegInit(VecInit(Seq.fill(BackendConfig.physicalRegNum)(0.U(32.W))))

  for (i <- 0 until readPortNum) {
    io.reads(i).value := regs(io.reads(i).id)
  }

}
