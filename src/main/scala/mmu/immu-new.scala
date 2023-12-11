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
    val vaddr = Output(UInt(BusConfig.ADDR_WIDTH))
    val paddr = Valid(UInt(BusConfig.ADDR_WIDTH))
    val exception = Output(new Exception)
  })

  val busIO = IO(BusMasterInterface())

  val ctrlIO = IO(new Bundle {
    // TODO: sfence.vma 更精细的清空TLB
    val clearTLB = Input(Bool())
  })

  val satp = WireInit(0.U.asTypeOf(new csr_satp_t))
  val privilege = WireInit(0.U(2.W))

  BoringUtils.addSink(satp, "satp")
  BoringUtils.addSink(privilege, "globalPrivilegeLevel")

  val tlb = Module(new TranslationLookasideBuffer)
  
  tlb.ctrlIO.clear := ctrlIO.clearTLB
  
  val walkResultValid = RegInit(false.B)
  val walkResultTag = RegInit(0.U.asTypeOf(new TLBTag))
  val walkResult = RegInit(0.U.asTypeOf(new PageTableEntry))
  val walkException = RegInit(0.U.asTypeOf(new Exception))

  when (ctrlIO.clearTLB) {
    walkResultValid := false.B
  }

  // F1
  tlb.io.search := f1_io.vaddr.GetTag(satp.asid)

  // F2
  val f2_vaddr = RegNext(f1_io.vaddr)
  val f2_tag = f2_vaddr.GetTag(satp.asid)

  val walkRequest = WireInit(false.B)

  f2_io.vaddr := f2_vaddr.asUInt

  when(privilege === 3.U || satp.mode === 0.U) {
    // M态，不启用地址翻译
    f2_io.paddr.valid := true.B
    f2_io.paddr.bits := f2_vaddr.asUInt

    // TODO : 地址检查
    f2_io.exception.valid := false.B
    f2_io.exception.code := DontCare

  }.otherwise {
    // 启用地址翻译

    when(tlb.io.result.hit) {
      // tlb hit，说明一定valid
      val entry = tlb.io.result.pte
      f2_io.paddr.valid := true.B
      f2_io.paddr.bits := Cat(entry.ppn1, entry.ppn0, f2_vaddr.offset)(31, 0)

      // TODO : 判断权限
      f2_io.exception := CheckFetchAddress(f2_io.paddr.bits, entry, privilege)

    }.otherwise {
      // tlb 没有hit，判断walkResult
      f2_io.paddr.valid := false.B
      f2_io.paddr.bits := DontCare

      when(walkResultValid && walkResultTag === f2_tag) {
        // walkResult 有效，且tag匹配，说明walk已经完成，而且没有写入tlb，说明发生缺页异常
        f2_io.exception := walkException
      }.otherwise {
        // walkResult 无效，或者tag不匹配，说明需要walk
        f2_io.exception.valid := false.B
        f2_io.exception.code := DontCare

        walkRequest := true.B
        walkResultTag := f2_tag
        walkResultValid := false.B
        walkException.valid := false.B

      }
    }

  }

  // WALK
  val List(idle, level1, level2, level3, over) = Enum(5)

  val walkState = RegInit(idle)
  busIO.master_turn_off()

  tlb.io.insert.tag := DontCare
  tlb.io.insert.entry := DontCare
  tlb.io.insert.submit := false.B

  switch(walkState) {
    is(idle) {
      busIO.addr := Cat(satp.ppn, f2_tag.vpn1, 0.U(2.W))(31, 0)
      when (walkRequest) {
        when(CheckValidRamAddress(busIO.addr)){
          walkState := level1
          busIO.stb := true.B
        }.otherwise{
          walkResultValid := true.B
          walkException.valid := true.B
          walkException.code := EC_IA_FAULT
        }       
      }
    }
    is(level1) {
      busIO.addr := Cat(satp.ppn, walkResultTag.vpn1, 0.U(2.W))(31, 0)
      busIO.stb := true.B
      when (busIO.ack) {
        walkState := level2
      }
    }
    is(level2) {
      val walkResultWire = busIO.dataRead.asTypeOf(new PageTableEntry)
      busIO.addr := Cat(walkResultWire.ppn1, walkResultWire.ppn0, walkResultTag.vpn0, 0.U(2.W))(31, 0)
      walkResult := walkResultWire

      when (walkResultWire.V) {
        walkState := level3
        busIO.stb := true.B
        when(CheckValidRamAddress(busIO.addr)){
          walkState := level1
          busIO.stb := true.B
        }.otherwise{
          walkResultValid := true.B
          walkException.valid := true.B
          walkException.code := EC_IA_FAULT
          state := idle
        }       
      } .otherwise {
        walkResultValid := true.B
        walkState := idle
      }

    }
    is(level3) {
      busIO.addr := Cat(walkResult.ppn1, walkResult.ppn0, walkResultTag.vpn0, 0.U(2.W))(31, 0)
      busIO.stb := true.B
      when (busIO.ack) {
        walkState := over
      }
    }
    is(over) {
      walkResultValid := true.B
      walkState := idle

      val walkResultWire = busIO.dataRead.asTypeOf(new PageTableEntry)
      walkResult := walkResultWire

      when (walkResultWire.V) {
        tlb.io.insert.tag := walkResultTag
        tlb.io.insert.entry := walkResultWire
        tlb.io.insert.submit := true.B
      }

    }
  }

}
