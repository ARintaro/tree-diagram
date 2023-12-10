package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import javax.swing.text.StyledEditorKit.BoldAction

class ExternalUartInterface extends Bundle {
  val txd = Output(Bool())
  val rxd = Input(Bool())
}

class UartControllerBlackBox extends BlackBox with HasBlackBoxResource {
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

class UartController extends Module {
  val io = IO(new Bundle {
    val bus = new BusSlaveInterface
  })

  if (GenConfig.innerUartModel) {
    val busy = RegInit(false.B)

    io.bus.dataRead := DontCare
    io.bus.ack := false.B
    io.bus.mmio := true.B

    val dataReady = RegInit(false.B)
    val data = RegInit(0.U(8.W))

    val dataWire = Wire(UInt(8.W))
    val startWire = Wire(Bool())
    
    BoringUtils.addSink(dataWire, "uart_data")
    BoringUtils.addSink(startWire, "uart_start")
    BoringUtils.addSource(dataReady, "uart_busy")

    when (startWire) {
      assert(!dataReady)
      dataReady := true.B
      data := dataWire
    }

    val dataOut = WireInit(0.U(8.W))
    val dataOutReady = WireInit(false.B)

    dontTouch(dataOut)
    dontTouch(dataOutReady)

    when (busy) {
      busy := false.B
      io.bus.ack := true.B
      when (io.bus.dataMode) {
        // Write
        when (io.bus.addr === BusConfig.UART_START.U) {
          dataOut := io.bus.dataWrite(7, 0)
          dataOutReady := true.B
          printf(cf"Uart Receive Data 0x$data%x, assci $data%c\n")

        } .elsewhen(io.bus.addr === (BusConfig.UART_START + 4).U) {
          assert(io.bus.dataWrite === 0.U, "read only")
        } .otherwise {
          assert(false.B, "addr error")
        }
      } .otherwise {
        // Read
        when (io.bus.addr === BusConfig.UART_START.U) {
          io.bus.dataRead := data
          dataReady := false.B
        } .elsewhen(io.bus.addr === (BusConfig.UART_START + 4).U) {
          io.bus.dataRead := Cat(0.U(8.W), 0.U(8.W), 0x20.U | dataReady, 0.U(8.W))
        } .otherwise {
          assert(false.B, "uart addr error")
        }
      }
    } .otherwise {
      when (io.bus.stb) {
        busy := true.B
      }
    }

  } else {
    val box = Module(new UartControllerBlackBox)

    val rxd = Wire(Bool())
    val txd = Wire(Bool())

    BoringUtils.addSink(rxd, "uart_rxd")
    BoringUtils.addSource(txd, "uart_txd")

    box.io.uart_rxd_i := rxd
    txd := box.io.uart_txd_o

    when(io.bus.stb && io.bus.dataMode) {
      DebugUtils.Print(cf"Uart Bus Write ${io.bus.dataWrite}")
    }

    box.io.clk_i := clock
    box.io.rst_i := reset
    box.io.wb_cyc_i := true.B
    box.io.wb_adr_i := io.bus.addr
    box.io.wb_dat_i := io.bus.dataWrite
    box.io.wb_sel_i := io.bus.dataBytesSelect
    box.io.wb_we_i := io.bus.dataMode
    io.bus.dataRead := box.io.wb_dat_o
    io.bus.mmio := true.B

    box.io.wb_stb_i := io.bus.stb && !io.bus.ack
    io.bus.ack := box.io.wb_ack_o


  }
}

class UartWithArbiter(inputNum: Int) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(inputNum, BusSlaveInterface())
  })

  val uart = Module(new UartController)
  val arbiter = Module(new BusArbiter(inputNum))

  io.masters <> arbiter.io.masters
  uart.io.bus <> arbiter.io.device

  require(inputNum >= 2)
}
