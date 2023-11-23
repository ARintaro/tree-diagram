package core

import chisel3._
import chisel3.util._

class InstructionFetchUnit extends Module {
  val io = IO(new Bundle {
    val fetch = Vec(FrontendConfig.decoderNum, Decoupled(new RawInstruction()))
  })

  val sramBusIO = IO(Vec(2, BusMasterInterface()))
  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())

    val clearTLB = Input(Bool())

    val clearIcache = Input(Bool())
  })

  val pc = Module(new ProgramCounter(1, 0x80000000L))

  val tlb = Module(new TranslationLookasideBuffer)
  val icache = Module(new InstructionCache(CacheConfig.icache))

  val fetchQueue = Module(
    new CircularQueue(
      new RawInstruction,
      FrontendConfig.fetchQueueSize,
      CacheConfig.icache.cacheLineSize,
      FrontendConfig.decoderNum,
      "FetchQueue"
    )
  )

  //暂时不处理flush
  fetchQueue.ctrlIO.flush := false.B
  tlb.ctrlIO.clear := ctrlIO.clearTLB
  icache.ctrlIO.clear := ctrlIO.clearIcache

  icache.ctrlIO.flush := ctrlIO.flush
  tlb.ctrlIO.flush := ctrlIO.flush

  sramBusIO(0) <> icache.sramIO
  sramBusIO(1) <> tlb.sramIO

  // 取指一阶段
  tlb.f1_io.vaddr := pc.io.vaddr
  icache.f1_io.vaddr := pc.io.vaddr

  // 取指二阶段
  icache.f2_io.paddr := tlb.f2_io.paddr

  // 在取指二阶段进行快速解码
  val fetchPC =
    (0 until CacheConfig.icache.cacheLineSize).map(i => tlb.f2_io.paddr.bits + (i * 4).U)
  val preDecs = VecInit(
    Seq.fill(CacheConfig.icache.cacheLineSize)(Module(new PreDecoder).io)
  )
  for (i <- 0 until CacheConfig.icache.cacheLineSize) {
    val curPC = tlb.f2_io.paddr.bits + (i * 4).U
    preDecs(i).inst := icache.f2_io.ins(i)
    preDecs(i).vaddr := curPC
  }

  // 判断是否有JAL，有的话更改发起重定向请求
  // TODO : 分支预测
  val isJal = preDecs.map(_.jumpType === JumpType.jal)
  val jalScan = isJal.scanLeft(false.B)(_ || _).tail
  val jalValid = (false.B +: jalScan.init).map(!_)

  // 计算最终的valid
  val fetchValid =
    jalValid.zip(icache.f2_io.valid).zip(fetchQueue.io.enq.map(_.ready)).map {
      case ((x, y), z) => x && y && z
    }
  val anyValid = fetchValid.reduce(_ || _)
  val lastValidIdx = (fetchValid.length - 1).U - PriorityEncoder(fetchValid.reverse)

  when(anyValid) {
    // 取出有效指令
    pc.io.reqs(0).valid := true.B
    pc.io.reqs(0).bits := preDecs(lastValidIdx).newVaddr
  }.otherwise {
    pc.io.reqs(0).valid := false.B
    pc.io.reqs(0).bits := DontCare
  }

  for (i <- 0 until CacheConfig.icache.cacheLineSize) {
    fetchQueue.io.enq(i).valid := fetchValid(i)
    fetchQueue.io.enq(i).bits.vaddr := fetchPC(i)
    fetchQueue.io.enq(i).bits.inst := icache.f2_io.ins(i)
  }

  fetchQueue.io.deq.zip(io.fetch).foreach {
    case (deq, fetch) => fetch <> deq
  }
}

class IfuTestTop extends Module {
  val io = IO(new Bundle {
    val dataRead = Output(UInt(BusConfig.DATA_WIDTH))
  })
  val sramBusIO = IO(Vec(2, BusMasterInterface()))
  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())

    val clearTLB = Input(Bool())

    val clearIcache = Input(Bool())
  })

  val ifu = Module(new InstructionFetchUnit)

  ifu.ctrlIO <> ctrlIO
  ifu.sramBusIO <> sramBusIO

  val fetched = Reg(Vec(FrontendConfig.decoderNum, new RawInstruction))
  val fetchedValid = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(false.B)))

  for (i <- 0 until FrontendConfig.decoderNum) {
    ifu.io.fetch(i).ready := true.B
    fetchedValid(i) := ifu.io.fetch(i).valid
    when (ifu.io.fetch(i).valid) {
      fetched(i) := ifu.io.fetch(i).bits
    }
  }

  io.dataRead := fetched(0).inst
}