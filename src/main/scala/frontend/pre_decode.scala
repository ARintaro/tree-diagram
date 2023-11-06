package core

import chisel3._
import chisel3.util._

object JumpType extends ChiselEnum {
  // 不会发生跳转
  val none = Value(0.U)
  // 一定跳转，可立即计算出目标地址
  val jal = Value(1.U)
  // 一定跳转，目标地址需要计算/预测
  val jalr = Value(2.U)
  //  可能跳转，可立即计算出目标地址
  val branch = Value(3.U)
}

class PreDecoder extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(InsConfig.INS_WIDTH))
    val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))

    val jumpType = Output(JumpType())
    val newVaddr = Output(UInt(BusConfig.ADDR_WIDTH))
  })

  val opcode = io.inst(6, 0)
  val data = io.inst

  // 默认为None
  io.jumpType := JumpType.none
  io.newVaddr := io.vaddr + 4.U

  val immTypeJ =
    Cat(data(31), data(19, 12), data(20), data(30, 25), data(24, 21), 0.U(1.W))
  val immTypeI = Cat(Fill(21, data(31)), data(30, 25), data(24, 21), data(20))
  val immTypeB =
    Cat(Fill(20, data(31)), data(7), data(30, 25), data(11, 8), 0.U(1.W))

  switch(opcode) {
    is(InsConfig.Opcode.jal) {
      io.jumpType := JumpType.jal
      io.newVaddr := io.vaddr + immTypeJ
    }
    is(InsConfig.Opcode.jalr) {
      io.jumpType := JumpType.jalr
    }
    is(InsConfig.Opcode.branch) {
      io.jumpType := JumpType.branch
      io.newVaddr := io.vaddr + immTypeB
    }
  }

}
