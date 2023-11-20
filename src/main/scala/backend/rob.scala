package core

import chisel3._
import chisel3.util._


class RobEntry extends Bundle {
  val valid = Bool()
  val vaddr = UInt(BusConfig.ADDR_WIDTH)

}

class NewRobRequest extends Bundle {
  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))

  val valid = Output(Bool())
  val idx = Output(UInt(BackendConfig.robSize.W))
}


class ReorderBuffer extends Module {
  

  val entry = RegInit(VecInit(Seq.fill(BackendConfig.robSize)(0.U.asTypeOf(new RobEntry))))

  val head = RegInit(0.U(BackendConfig.robIdxWidth))
  val tail = RegInit(0.U(BackendConfig.robIdxWidth))

  

}