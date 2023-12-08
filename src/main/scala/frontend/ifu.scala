package core

import chisel3._
import chisel3.util._

class InstructionFetchUnit extends Module {
  val io = IO(new Bundle {
    val fetch = Vec(FrontendConfig.decoderNum, Decoupled(new RawInstruction()))

    val redirect = Vec(2, Flipped(Valid(UInt(BusConfig.ADDR_WIDTH))))
  })

  val sramBusIO = IO(Vec(2, BusMasterInterface()))
  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())

    val clearTLB = Input(Bool())

    val clearIcache = Input(Bool())
  })

  val pc = Module(new ProgramCounter(3, 0x80000000L))

  // ROB发来的重定向请求
  pc.io.reqs(1) <> io.redirect(0)

  // exceptionUnit发来的重定向请求
  pc.io.reqs(2) <> io.redirect(1)

  assert(!io.redirect(0).valid || ctrlIO.flush, "Redirect without flush")

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

  for (i <- 0 until CacheConfig.icache.cacheLineSize) {
    fetchQueue.io.enq(i).bits.exception := false.B
    fetchQueue.io.enq(i).bits.exceptionCode := 0.U
  }

  if (DebugConfig.printFetch) {
    for(i <- 0 until CacheConfig.icache.cacheLineSize) {
      when(fetchQueue.io.enq(i).ready && fetchQueue.io.enq(i).valid) {
        val bits = fetchQueue.io.enq(i).bits
        DebugUtils.Print(cf"fetched inst 0x${bits.inst}%x vaddr 0x${bits.vaddr}%x")
      }
    }
  }

  fetchQueue.ctrlIO.flush := ctrlIO.flush
  tlb.ctrlIO.clear := ctrlIO.clearTLB
  icache.ctrlIO.clear := ctrlIO.clearIcache

  // flush时 fetchQueue会拒绝输入，icache无所谓flush
  icache.ctrlIO.flush := false.B
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

  // 判断是否有Jump，有的话更改发起重定向请求
  // TODO : 分支预测
  val isJump = preDecs.map(_.jump)
  val jumpScan = isJump.scanLeft(false.B)(_ || _).tail
  val jumpValid = (false.B +: jumpScan.init).map(!_)

  // 计算最终的valid
  val fetchValid =
    jumpValid.zip(icache.f2_io.valid).zip(fetchQueue.io.enq.map(_.ready)).map {
      case ((x, y), z) => x && y && z
    }.scanLeft(true.B)(_ && _).tail

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
    fetchQueue.io.enq(i).bits.jump := Mux(preDecs(i).jumpType === JumpType.jal, false.B, preDecs(i).jump)
    fetchQueue.io.enq(i).bits.jumpTarget := preDecs(i).newVaddr
  }

  fetchQueue.io.deq.zip(io.fetch).foreach {
    case (deq, fetch) => fetch <> deq
  }
}

