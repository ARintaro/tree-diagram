package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class ExternalVGAInterface extends Bundle {
  val videoRed = Output(UInt(3.W))
  val videoGreen = Output(UInt(3.W))
  val videoBlue = Output(UInt(2.W))
  val videoHsync = Output(Bool())
  val videoVsync = Output(Bool())
  val videoClk = Input(Clock())
  val videoDe = Output(Bool())
}

class VGAIndex extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
      val clk = Input(Clock())
      val hsync = Output(Bool())
      val vsync = Output(Bool())
      val hdata = Output(UInt(12.W))
      val vdata = Output(UInt(12.W))
      val data_enable = Output(Bool())
  })

  addResource("vga_index.sv")
}

class VGAController extends Module {
  val io = IO(new Bundle {
    val bus = new BusSlaveInterface
  })

  val externalInterface = Wire(new ExternalVGAInterface)
  BoringUtils.addSource(externalInterface.videoRed, "vga_videoRed")
  BoringUtils.addSource(externalInterface.videoGreen, "vga_videoGreen")
  BoringUtils.addSource(externalInterface.videoBlue, "vga_videoBlue")
  BoringUtils.addSource(externalInterface.videoHsync, "vga_videoHsync")
  BoringUtils.addSource(externalInterface.videoVsync, "vga_videoVsync")
  BoringUtils.addSink(externalInterface.videoClk, "vga_videoClk")
  BoringUtils.addSource(externalInterface.videoDe, "vga_videoDe")

  val config = new BramConfig(
    VGAConfig.vramWriteWidth,
    VGAConfig.vramWriteDepth,
    VGAConfig.vramReadWidth
  )
  val vram = Module(new SimpleDualPortBram("VRAM", config))
  BramConfig.map.update("VRAM", config)
  vram.io.clka := clock
  vram.io.addra := io.bus.addr(config.writeAddrWidth + 1, 2)
  vram.io.dina := io.bus.dataWrite
  vram.io.wea := io.bus.stb && io.bus.dataMode
  vram.io.clkb := externalInterface.videoClk

  io.bus.ack := RegNext(io.bus.stb)
  io.bus.dataRead := 0.U
  io.bus.mmio := true.B

  val rgb = vram.io.doutb
  externalInterface.videoRed := rgb(7, 5)
  externalInterface.videoGreen := rgb(4, 2)
  externalInterface.videoBlue := rgb(1, 0)

  val vgaIndex = Module(new VGAIndex)
  vgaIndex.io.clk := externalInterface.videoClk
  externalInterface.videoHsync := vgaIndex.io.hsync
  externalInterface.videoVsync := vgaIndex.io.vsync
  externalInterface.videoDe := vgaIndex.io.data_enable

  val compressionRatio = VGAConfig.compressionRatio
  vram.io.addrb := (vgaIndex.io.hdata >> log2Ceil(compressionRatio)) + (100.U * (vgaIndex.io.vdata >> log2Ceil(compressionRatio)))

//   val rgb = Wire(UInt(8.W))
//   rgb := 0.U
//   when(vgaIndex.io.index < 200000.U && vgaIndex.io.hdata < 266.U) {
//     rgb := "b00000011".U
//   } .elsewhen(vgaIndex.io.index >= 200000.U && vgaIndex.io.index < 400000.U && vgaIndex.io.hdata < 536.U && vgaIndex.io.hdata > 266.U) {
//     rgb := "b00011100".U
//   } .elsewhen(vgaIndex.io.index >= 400000.U && vgaIndex.io.hdata >= 536.U) {
//     rgb := "b11100000".U
//   }
//   externalInterface.videoRed := rgb(7, 5)
//   externalInterface.videoGreen := rgb(4, 2)
//   externalInterface.videoBlue := rgb(1, 0)
}

class VGAWithArbiter(inputNum: Int) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(inputNum, BusSlaveInterface())
  })

  val vga = Module(new VGAController)
  val arbiter = Module(new BusArbiter(inputNum))

  io.masters <> arbiter.io.masters
  vga.io.bus <> arbiter.io.device

  require(inputNum >= 2)
}

class VGAtestTop extends Module {
    val io = IO(new Bundle{
        val vga = new ExternalVGAInterface
        val dip_sw = Input(UInt(32.W))
        val button = Input(Bool())
    })
    DeviceUtils.AddExternalVGA(io.vga)
    val posedge = io.button & !RegNext(io.button)
    val vga = Module(new VGAController)
    vga.io.bus.addr := io.dip_sw(31, 16)
    vga.io.bus.dataWrite := "h33333333".U
    vga.io.bus.dataMode := true.B 
    vga.io.bus.dataBytesSelect := "b1111".U
    vga.io.bus.stb := false.B
    when(posedge) {
        vga.io.bus.stb := true.B
    }
}