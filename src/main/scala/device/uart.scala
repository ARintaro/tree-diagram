package core

import chisel3._
import chisel3.util._

class ExternalUartInterface extends Bundle {
  val txd = Output(Bool())
  val rxd = Input(Bool())
}

class UartController extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk_i = Input(Clock())
    val rst_i = Input(Reset())

    val wb_cyc_i = Input(Bool())
    val wb_stb_i = Input(Bool())
    val wb_ack_o = Output(Bool())
    val wb_adr_i = Input(UInt(BusConfig.ADDR_WIDTH))
    val wb_dat_i = Input(UInt(BusConfig.DATA_WIDTH))
    val wb_dat_o = Output(UInt(BusConfig.DATA_WIDTH))
    val wb_sel_i = Input(UInt((BusConfig.DATA_WIDTH.get / 8).W))
    val wb_we_i = Input(Bool())

    val uart_txd_o = Output(Bool())
    val uart_rxd_i = Input(Bool())
  })

  addResource("uart_controller.sv")
}


