package test

import chisel3._ 
import chisel3.util._ 
import core._ 
import BusConfig._

class general_tb extends Module {
    val Ifu = Module(new InstructionFetchUnit)
    val sram0 = IO(new ExternalSramInterface)
    val backend = Module(new Backend)
    val decoder = Module(new DecodeUnit)

    SramUtils.AddExternalSram(sram0, "testSram0")
    val SramController = Module(new SramWithArbiter("testSram0", 4))

    // 四根总线
    val Buses = Seq.fill(4)(Module(new BusMux(1)))
    // 将Buses的输出连接到SramController的输入
    for(i <- 0 until 4) {
        Buses(i).io.slaves(0) <> SramController.io.masters(i)
    }
    // 接入地址分配
    for(i <- 0 until 4) {
        Buses(i).io.allocate(0).start := BASE_RAM_START.U
        Buses(i).io.allocate(0).mask := BASE_RAM_MASK.U
    }
    // 分别来自 ifu(tlb & icache), backend(storebuffer & mem_pipeline)
    Buses(0).io.master <> Ifu.sramBusIO(0)
    Buses(1).io.master <> Ifu.sramBusIO(1)
    Buses(2).io.master <> backend.io.memBus(0)
    Buses(3).io.master <> backend.io.memBus(1)

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

// class general_top extends Module {
//     val tb = Module(new general_tb)
//     val virtualSram = Module(new virtualSram)
//     tb.sram0 <> virtualSram.io
// }