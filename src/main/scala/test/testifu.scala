package test

import chisel3._
import chisel3.util._
import core._

class IfutestTop extends Module {
  val sram0 = IO(new ExternalSramInterface)
  val InstrIO = IO(new Bundle {
    val fetch = Vec(FrontendConfig.decoderNum, Decoupled(new RawInstruction()))
  })
  SramUtils.AddExternalSram(sram0, "testSram0")
  val SramController = Module(new SramWithArbiter("testSram0", 2))
  val ifu = Module(new InstructionFetchUnit)
  ifu.sramBusIO(0) <> SramController.io.masters(0)
  ifu.sramBusIO(1) <> SramController.io.masters(1)
  ifu.ctrlIO.clearIcache := false.B
  ifu.ctrlIO.clearTLB := false.B
  ifu.ctrlIO.flush := false.B
  InstrIO.fetch <> ifu.io.fetch
}
