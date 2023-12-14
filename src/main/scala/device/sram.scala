package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class ExternalSramInterface extends Bundle {
  val addr = Output(UInt(SramConfig.ADDR_WIDTH))
  val dataRead = Input(UInt(SramConfig.DATA_WIDTH))
  val dataWrite = Output(UInt(SramConfig.DATA_WIDTH))
  val readDisable = Output(Bool())
  val writeDisable = Output(Bool())
  val bytesDisable = Output(Bits(SramConfig.DATA_BYTES_NUM))
}

class ExternalSram(name : String) extends Module {
  val io = IO(Flipped(new ExternalSramInterface))

  BoringUtils.addSource(io.addr, f"${name}_addr")
  BoringUtils.addSink(io.dataRead, f"${name}_dataRead")
  BoringUtils.addSource(io.dataWrite, f"${name}_dataWrite")
  BoringUtils.addSource(io.readDisable, f"${name}_readDisable")
  BoringUtils.addSource(io.writeDisable, f"${name}_writeDisable")
  BoringUtils.addSource(io.bytesDisable, f"${name}_bytesDisable")
}

class Sram(name : String) extends Module {
  val busIO = IO(new BusSlaveInterface)

  val externSram = Module(new ExternalSram(name))

  val idle :: reading :: writting :: writing2 :: Nil = Enum(3)
  var state = RegInit(idle)

  externSram.io.dataWrite := 0.U
  externSram.io.readDisable := true.B
  externSram.io.writeDisable := true.B
  externSram.io.bytesDisable := DontCare

  externSram.io.addr := busIO.addr(21, 2)
  externSram.io.bytesDisable := ~busIO.dataBytesSelect

  busIO.ack := false.B
  busIO.mmio := false.B
  busIO.dataRead := externSram.io.dataRead

  switch(state) {
    is (idle) {
      
      when (busIO.stb) {
        // 收到请求
        when (busIO.dataMode) {
          // write
          externSram.io.dataWrite := busIO.dataWrite
          externSram.io.writeDisable := false.B
          state := writting
        } .otherwise {
          // read
          externSram.io.readDisable := false.B
          state := reading
        }
      }
    }
    is (reading) {
      externSram.io.readDisable := false.B
      busIO.ack := true.B 

      state := idle
    }
    is (writting) {
      busIO.ack := true.B
      externSram.io.dataWrite := busIO.dataWrite

      state := idle
    }
  }
}

class SramWithArbiter(name : String, inputNum : Int) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(inputNum, BusSlaveInterface())
  })

  val sram = Module(new SramController(name))

  val arbiter = Module(new BusArbiter(inputNum))

  io.masters <> arbiter.io.masters
  arbiter.io.device <> sram.busIO

  require (inputNum >= 2)
}

object DeviceUtils {
  
  // TODO : 反射
  def AddExternalSram(io : ExternalSramInterface, name : String) : Unit = {
    BoringUtils.addSink(io.addr, f"${name}_addr")
    BoringUtils.addSource(io.dataRead, f"${name}_dataRead")
    BoringUtils.addSink(io.dataWrite, f"${name}_dataWrite")
    BoringUtils.addSink(io.readDisable, f"${name}_readDisable")
    BoringUtils.addSink(io.writeDisable, f"${name}_writeDisable")
    BoringUtils.addSink(io.bytesDisable, f"${name}_bytesDisable")
  }

  def AddExternalUart(io : ExternalUartInterface) : Unit = {
    BoringUtils.addSource(io.rxd, "uart_rxd")
    BoringUtils.addSink(io.txd, "uart_txd")
  }

  def AddExternalVGA(io : ExternalVGAInterface) : Unit = {
    BoringUtils.addSink(io.videoRed, "vga_videoRed")
    BoringUtils.addSink(io.videoGreen, "vga_videoGreen")
    BoringUtils.addSink(io.videoBlue, "vga_videoBlue")
    BoringUtils.addSink(io.videoHsync, "vga_videoHsync")
    BoringUtils.addSink(io.videoVsync, "vga_videoVsync")
    BoringUtils.addSource(io.videoClk, "vga_videoClk")
    BoringUtils.addSink(io.videoDe, "vga_videoDe")
  }
}