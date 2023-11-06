package core

import chisel3._
import chisel3.util._

class InstructionFetchUnit extends Module {
  val io = IO(new Bundle {
	val sramBus = Vec(2, BusMasterInterface())
  })

  val ctrlIO = IO(new Bundle {
	val flush = Input(Bool())

	val clearTLB = Input(Bool())
	
	val clearIcache = Input(Bool())
  })
 
  val pc = Module(new ProgramCounter(2, 0x80000000))

  val tlb = Module(new TranslationLookasideBuffer)
  val icache = Module(new InstructionCache(CacheConfig.icache))

  tlb.ctrlIO.clear := ctrlIO.clearTLB
  icache.ctrlIO.clear := ctrlIO.clearIcache

  icache.ctrlIO.flush := ctrlIO.flush
  tlb.ctrlIO.flush := ctrlIO.flush
  
  io.sramBus(0) := icache.sramIO
  io.sramBus(1) := tlb.sramIO


  // 取指一阶段
  tlb.f1_io.vaddr := pc.io.vaddr
  icache.f1_io.vaddr := pc.io.vaddr

  // 取指二阶段
  icache.f2_io.paddr := tlb.f2_io.paddr

  // 在取指二阶段进行快速解码
  val fetchPC = (0 until CacheConfig.icache.cacheLineSize).map(i => pc.io.vaddr + (i * 4).U)
  val preDecs = VecInit(Seq.fill(CacheConfig.icache.cacheLineSize)(Module(new PreDecoder).io))
  for (i <- 0 until CacheConfig.icache.cacheLineSize) {
	val curPC = tlb.f2_io.paddr.bits + (i * 4).U
	preDecs(i).inst := icache.f2_io.ins(i)
	preDecs(i).vaddr := curPC
  }
  
  // 判断是否有JAL，有的话更改发起重定向请求
  // TODO : 分支预测
  
  



}
