package core

import chisel3._
import chisel3.util._
import os.stat

class TranslationLookasideBuffer extends Module {
  val f1_io = IO(new Bundle {
    // 请求的虚拟地址
    val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  })

  val f2_io = IO(new Bundle {
    // 上周期请求的虚拟地址对应的物理地址
    val paddr = Valid(UInt(BusConfig.ADDR_WIDTH))
    // 上周期请求的虚拟地址，用于产生重定向地址
    val vaddr = Output(UInt(BusConfig.ADDR_WIDTH))
  })

  val ctrlIO = IO(Input(new CtrlInterface))

  val sramIO = IO(BusMasterInterface())

  // TODO : 实现TLB

  val state = RegInit(false.B)

  state := true.B

  val lastVaddr = RegNext(f1_io.vaddr)

  f2_io.paddr.valid := state && !ctrlIO.clear
  f2_io.paddr.bits := lastVaddr
  f2_io.vaddr := lastVaddr
}
