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

class TestInter extends Bundle {
  val in = Input(Bool())
  val out = Output(UInt(3.W))
}


class TestTop extends Module {
  val io = IO(new TestInter())

  var state = Wire(Vec(5, UInt(3.W)))
  state.foreach(_ := 0.U)

  for (i <- 0 until 5) {
    val newState = Wire(Vec(5, UInt(3.W)))
    for (j <- 0 until 5) {
      newState(j) := state(j) + 1.U
    }

    state = newState
  }

  io.out := state(1)

}

class VivadoTop extends Module {
  val Ifu = Module(new InstructionFetchUnit)
    val sram0 = IO(new ExternalSramInterface)
    val backend = Module(new Backend)
    val decoder = Module(new DecodeUnit)

    SramUtils.AddExternalSram(sram0, "baseSram")
    val SramController = Module(new SramWithArbiter("baseSram", 4))

    // 四根总线
    val Buses = Seq.fill(4)(Module(new BusMux(1)))
    // 将Buses的输出连接到SramController的输入
    for(i <- 0 until 4) {
        Buses(i).io.slaves(0) <> SramController.io.masters(i)
    }
    // 接入地址分配
    for(i <- 0 until 4) {
        Buses(i).io.allocate(0).start := BusConfig.BASE_RAM_START.U
        Buses(i).io.allocate(0).mask := BusConfig.BASE_RAM_MASK.U
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
}