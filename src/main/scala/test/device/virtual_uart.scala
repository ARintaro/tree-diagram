package test

import chisel3._
import chisel3.util._

class UartModel extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rxd = Input(Bool())
    val txd = Output(Bool())
    val start = Input(Bool())
    val data = Input(UInt(8.W))
    val busy = Output(Bool())
  })

  addResource("uart_model.sv")
}

