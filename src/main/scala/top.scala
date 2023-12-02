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

class TreeDiagram extends Module {
  val io = IO(new Bundle {
    val baseRam = new ExternalSramInterface
    val extRam = new ExternalSramInterface
  })

  val ifu = Module(new InstructionFetchUnit)
  val backend = Module(new Backend)
  val decoder = Module(new DecodeUnit)

  SramUtils.AddExternalSram(io.baseRam, "baseRam")
  SramUtils.AddExternalSram(io.extRam, "extRam")

  val memBusNum = 2
  val devBusNum = 2
  val busNum = memBusNum + devBusNum

  val baseRam = Module(new SramWithArbiter("baseRam", busNum))
  val extRam = Module(new SramWithArbiter("extRam", busNum))

  // 两条纯内存总线
  // 0: icache
  // 1: ifu tlb

  val memBuses = Seq.fill(2)(Module(new BusMux(2)))

  // 将Buses的输出连接到SramController的输入
  for (i <- 0 until 2) {
    memBuses(i).io.slaves(0) <> baseRam.io.masters(i)
    memBuses(i).io.allocate(0).start := BusConfig.BASE_RAM_START.U
    memBuses(i).io.allocate(0).mask := BusConfig.BASE_RAM_MASK.U

    memBuses(i).io.slaves(1) <> extRam.io.masters(i)
    memBuses(i).io.allocate(1).start := BusConfig.EXT_RAM_START.U
    memBuses(i).io.allocate(1).mask := BusConfig.EXT_RAM_MASK.U
  }

  ifu.sramBusIO(0) <> memBuses(0).io.master
  ifu.sramBusIO(1) <> memBuses(1).io.master

  // 带设备总线，用于mem_pipeline和storebuffer
  val devBuses = Seq.fill(2)(Module(new BusMux(2)))

  for (i <- 0 until 2) {
    devBuses(i).io.slaves(0) <> baseRam.io.masters(i + memBusNum)
    devBuses(i).io.allocate(0).start := BusConfig.BASE_RAM_START.U
    devBuses(i).io.allocate(0).mask := BusConfig.BASE_RAM_MASK.U

    devBuses(i).io.slaves(1) <> extRam.io.masters(i + memBusNum)
    devBuses(i).io.allocate(1).start := BusConfig.EXT_RAM_START.U
    devBuses(i).io.allocate(1).mask := BusConfig.EXT_RAM_MASK.U

    // TODO : uart
  }

  backend.io.devBus(0) <> devBuses(0).io.master
  backend.io.devBus(1) <> devBuses(1).io.master

  ifu.ctrlIO.clearIcache := false.B
  ifu.ctrlIO.clearTLB := false.B
  ifu.ctrlIO.flush := backend.ctrlIO.flushPipeline

  ifu.io.redirect(0) := backend.io.robRedirect
  ifu.io.fetch <> decoder.io.in
  decoder.io.out <> backend.io.in
  decoder.io.nextDone := backend.io.renameDone
  decoder.ctrlIO.flush := backend.ctrlIO.flushPipeline

  val debug = Module(new DebugModule)
}
