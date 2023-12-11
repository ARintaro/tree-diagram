package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._
import AddressException._

class InstructionMemoryManagementUnitNew extends Module {
  val f1_io = IO(new Bundle {
    val vaddr = Input(new VirtualAddress())
  })

  val f2_io = IO(new Bundle {
    val vaddr = Output(new VirtualAddress)
    val paddr = Valid(UInt(BusConfig.ADDR_WIDTH))
    val exception = Output(new Exception)
  })

  val busIO = IO(new BusMasterInterface)

  val ctrlIO = IO(new Bundle {
    // TODO: sfence.vma 更精细的清空TLB
    val clearTLB = Input(Bool())
  })

  val satp = WireInit(0.U.asTypeOf(new csr_satp_t))
  val privilege = WireInit(0.U(2.W))

  BoringUtils.addSink(satp, "satp")
  BoringUtils.addSink(privilege, "globalPrivilegeLevel")
  
  val tlb = Module(new TranslationLookasideBuffer)

  val walkResultValid = RegInit(false.B)
  val walkResultTag = RegInit(0.U.asTypeOf(new TLBTag))
  val walkResult = RegInit(0.U.asTypeOf(new PageTableEntry))
  val walkResultException = RegInit(0.U.asTypeOf(new Exception))

  // F1
  tlb.io.search := f1_io.vaddr.GetTag(satp.asid)

  // F2
  val f2_vaddr = RegNext(f1_io.vaddr)
  val f2_tag = f2_vaddr.GetTag(satp.asid)

  val walkRequest = WireInit(false.B)


  when (tlb.io.result.hit) {
    // tlb hit，说明一定valid，判断权限
  } .otherwise {
    // tlb 没有hit，判断walkResult
  }
  


  // WALK
  val List(idle, level1, level2) = Enum(3)

  val walkState = RegInit(idle)
  
  switch(walkState) {
    is (idle) {
      
    }
    is (level1) {

    }
    is (level2) {

    }
  }



  



}
