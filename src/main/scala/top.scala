package core

import chisel3._
import chisel3.util.experimental.BoringUtils

class TopInterface extends Bundle {
  val sram0 = new ExternalSramInterface
  // val sram1 = new ExternalSramInterface
}

class TestModule extends Module {

  val sram = Module(new ExternalSram("sram0"))

  sram.io.addr := 7.U
  sram.io.bytesDisable := 0.U
  sram.io.readDisable := false.B
  sram.io.writeDisable := false.B
  sram.io.dataWrite := 4.U
}

class Top extends Module {
  val io = IO(new TopInterface)

  SramUtils.AddExternalSram(io.sram0, "sram0")
  // SramUtils.AddExternalSram(io.sram1, "sram1")

  val sram = Module(new Sram("sram0"))

  sram.busIO.addr := 7.U
  sram.busIO.dataBytesSelect := 0.U
  sram.busIO.stb := true.B
  sram.busIO.dataMode := false.B
  sram.busIO.dataWrite := 4.U

  
}

class TestTop extends Module {
  val io = IO(new Bundle {
    val data = Output(UInt(4.W))
  })

  val test = RegInit(0.U(4.W))
  test := test + 1.U
  io.data := test
}

