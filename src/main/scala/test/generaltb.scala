package test

import chisel3._ 
import chisel3.util._ 
import core._ 

class general_tb extends Module {
    val Ifu = Module(new InstructionFetchUnit)
    val sram0 = IO(new ExternalSramInterface)
    val backend = Module(new Backend)
    val decoder = Module(new DecodeUnit)

    SramUtils.AddExternalSram(sram0, "testSram0")
    val SramController = Module(new SramWithArbiter("testSram0", 2))

    Ifu.sramBusIO(0) <> SramController.io.masters(0)
    Ifu.sramBusIO(1) <> SramController.io.masters(1)
    Ifu.ctrlIO.clearIcache := false.B
    Ifu.ctrlIO.clearTLB := false.B
    Ifu.ctrlIO.flush := backend.ctrlIO.flushPipeline

    Ifu.io.redirect(0) := backend.io.robRedirect
    Ifu.io.fetch <> decoder.io.in
    decoder.io.out <> backend.io.in
    decoder.io.nextDone := backend.io.renameDone
    decoder.ctrlIO.flush := backend.ctrlIO.flushPipeline

    val debug = Module(new DebugModule)
}

class general_top extends Module {
    val tb = Module(new general_tb)
    val virtualSram = Module(new virtualSram)
    tb.sram0 <> virtualSram.io
}