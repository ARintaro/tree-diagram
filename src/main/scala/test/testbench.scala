package test

import chisel3._
import chisel3.util._
import core._
import BusConfig._

class TestBench extends Module {
  val top = Module(new TreeDiagram)

  val sram0 = Module(new VirtualSram)
  val sram1 = Module(new VirtualSram)
  
  top.io.baseRam <> sram0.io
  top.io.extRam <> sram1.io

  if (!GenConfig.innerUartModel) {
    val uart = Module(new UartModel)
    uart.io.rxd := top.io.uart.txd
    top.io.uart.rxd := uart.io.txd
  } else {
    top.io.uart.rxd := DontCare
  }

  
}
