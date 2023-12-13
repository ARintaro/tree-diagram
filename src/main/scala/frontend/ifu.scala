package core

import chisel3._
import chisel3.util._

class RedirectRequest extends Bundle {
  val valid = Bool()
  val target = UInt(BusConfig.ADDR_WIDTH)
}

class InstructionFetchUnit extends Module {
  val io = IO(new Bundle {
    val fetch = Vec(FrontendConfig.decoderNum, Decoupled(new RawInstruction()))

  })

  val sramBusIO = IO(Vec(2, BusMasterInterface()))
  val ctrlIO = IO(new Bundle {
    val redirect = Input(new RedirectRequest)
    val clearTLB = Input(Bool())
    val flush = Input(Bool())
    val clearIcache = Input(Bool())
  })

  val pc = Module(new ProgramCounter(2, 0x80000000L))

  assert(!((ctrlIO.redirect.valid || ctrlIO.clearIcache || ctrlIO.clearTLB) && (!ctrlIO.flush)))

  pc.io.reqs(1) := ctrlIO.redirect


  val immu = Module(new InstructionMemoryManagementUnitNew)
  val icache = Module(new InstructionCache(CacheConfig.icache))

  immu.ctrlIO.flush := ctrlIO.flush

  val fetchQueue = Module(
    new CircularQueue(
      new RawInstruction,
      FrontendConfig.fetchQueueSize,
      CacheConfig.icache.cacheLineSize,
      FrontendConfig.decoderNum,
      "FetchQueue"
    )
  )

  fetchQueue.ctrlIO.flush := ctrlIO.flush
  immu.ctrlIO.clearTLB := ctrlIO.clearTLB
  icache.ctrlIO.clear := ctrlIO.clearIcache | ctrlIO.clearTLB

  // flush时 fetchQueue会拒绝输入，icache无所谓flush
  icache.ctrlIO.flush := false.B

  sramBusIO(0) <> icache.sramIO
  sramBusIO(1) <> immu.busIO

  // 取指一阶段
  immu.f1_io.vaddr := pc.io.vaddr.asTypeOf(new VirtualAddress)
  icache.f1_io.vaddr := pc.io.vaddr

  // 取指二阶段
  icache.f2_io.paddr := immu.f2_io.paddr
  icache.f2_io.vaddr := immu.f2_io.vaddr

  when (immu.f2_io.exception.valid === false.B) {
    // immu 未出现异常

    // 在取指二阶段进行快速解码
    val fetchPC = (0 until CacheConfig.icache.cacheLineSize).map(i => immu.f2_io.vaddr + (i * 4).U)

    // 判断是否有Jump，有的话更改发起重定向请求
    // TODO : 分支预测
    val isJump = icache.f2_io.ins.map(x => (x.jumpType === JumpType.branch || x.jumpType === JumpType.jal) )
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
      pc.io.reqs(0).target := icache.f2_io.ins(lastValidIdx).target << 2

      DebugUtils.Print(cf"Frontend Redirect ${pc.io.reqs(0).valid} addr 0x${pc.io.reqs(0).target}%x")

    }.otherwise {
      pc.io.reqs(0).valid := false.B
      pc.io.reqs(0).target := DontCare
    }

    for (i <- 0 until CacheConfig.icache.cacheLineSize) {
      fetchQueue.io.enq(i).valid := fetchValid(i)
      fetchQueue.io.enq(i).bits.vaddr := fetchPC(i)
      fetchQueue.io.enq(i).bits.inst := icache.f2_io.ins(i).inst
      fetchQueue.io.enq(i).bits.jump := icache.f2_io.ins(i).jumpType === JumpType.branch
      fetchQueue.io.enq(i).bits.jumpTarget := icache.f2_io.ins(i).target << 2
      fetchQueue.io.enq(i).bits.exception := false.B
      fetchQueue.io.enq(i).bits.exceptionCode := 0.U
    }
  } .otherwise {
    pc.io.reqs(0).valid := false.B
    pc.io.reqs(0).target := DontCare

    for (i <- 0 until CacheConfig.icache.cacheLineSize) {
      fetchQueue.io.enq(i).valid := false.B
      fetchQueue.io.enq(i).bits.vaddr := DontCare
      fetchQueue.io.enq(i).bits.inst := DontCare
      fetchQueue.io.enq(i).bits.jump := DontCare
      fetchQueue.io.enq(i).bits.jumpTarget := DontCare
      fetchQueue.io.enq(i).bits.exception := false.B
      fetchQueue.io.enq(i).bits.exceptionCode := 0.U
    }
    
    fetchQueue.io.enq(0).valid := true.B
    fetchQueue.io.enq(0).bits.vaddr := immu.f2_io.vaddr
    fetchQueue.io.enq(0).bits.inst := DontCare
    fetchQueue.io.enq(0).bits.jump := DontCare
    fetchQueue.io.enq(0).bits.jumpTarget := DontCare
    fetchQueue.io.enq(0).bits.exception := true.B
    fetchQueue.io.enq(0).bits.exceptionCode := immu.f2_io.exception.code
  }


  fetchQueue.io.deq.zip(io.fetch).foreach {
    case (deq, fetch) => fetch <> deq
  }

  if (DebugConfig.printFetch) {
    for(i <- 0 until CacheConfig.icache.cacheLineSize) {
      when(fetchQueue.io.enq(i).ready && fetchQueue.io.enq(i).valid) {
        val bits = fetchQueue.io.enq(i).bits
        DebugUtils.Print(cf"fetched inst 0x${bits.inst}%x vaddr 0x${bits.vaddr}%x")
      }
    }
  }

  if (DebugConfig.printFlush) {
    when (ctrlIO.flush) {
      DebugUtils.Print("Frontend Flushing")
    }
  }
}

