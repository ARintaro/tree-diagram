package core

import chisel3._
import chisel3.util._

class RawInstruction extends Bundle {
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val inst = UInt(InsConfig.INS_WIDTH)
}
