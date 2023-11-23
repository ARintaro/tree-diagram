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

class PhysicalRegisterFile(readPortNum: Int, writePortNum: Int) extends Module {
  val io = IO(new Bundle {
    val reads = Vec(readPortNum, Vec(2, Flipped(new RegisterReadRequest)))
    val writes = Vec(writePortNum, Flipped(new RegisterWriteRequest))
  })

  val regs = RegInit(VecInit(Seq.fill(BackendConfig.physicalRegNum)(0.U(32.W))))

  for (i <- 0 until readPortNum) {
    for (j <- 0 until 2) {
      io.reads(i)(j).value := regs(io.reads(i)(j).id)
    }
  }

  for (i <- 0 until writePortNum) {
    when(io.writes(i).valid) {
      regs(io.writes(i).id) := io.writes(i).value
    }
  }

}
