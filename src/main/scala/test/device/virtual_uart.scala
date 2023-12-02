package test

import chisel3._
import chisel3.util._

class UartModel extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val rxd = Input(Bool())
    val txd = Output(Bool())
  })

  addResource("uart_model.sv")
}

