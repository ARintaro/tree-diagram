package core

import chisel3._
import chisel3.util._

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._

class Exception extends Bundle {
  val valid = Bool()
  val code = UInt(InsConfig.EXCEPTION_WIDTH)
}

object AddressException {
  def invalidAddr(paddr: UInt): Bool = {
    val valid = WireInit(false.B)
    // 并行 分两个函数
    when((paddr & BASE_RAM_MASK.U) === BASE_RAM_START.U) {
      valid := true.B
    }.elsewhen((paddr & EXT_RAM_MASK.U) === EXT_RAM_START.U) {
      valid := true.B
    }.elsewhen((paddr & UART_MASK.U) === UART_START.U) {
      valid := true.B
    }.elsewhen((paddr & TIMER_MASK.U) === TIMER_START.U) {
      valid := true.B
    }.otherwise(valid := false.B)
    // FIXME: 之后还要补充对其他外设的判断
    !valid
  }

  def CheckValidRamAddress(paddr: UInt): Bool = {
    return  ( (paddr & BASE_RAM_MASK.U) === BASE_RAM_START.U ||
      (paddr & EXT_RAM_MASK.U) === EXT_RAM_START.U)
  }

  def CheckValidAddress(paddr: UInt): Bool = {
    return (CheckValidAddress(paddr) || (paddr & UART_MASK.U) === UART_START.U ||
      (paddr & TIMER_MASK.U) === TIMER_START.U) // TODO: 其他外设
  }

  def CheckFetchAddress(paddr: UInt, entry: PageTableEntry, privilege: PrivilegeLevel): Exception = {
    val exception = WireInit(0.U.asTypeOf(new Exception))
    val misaligned = (paddr(1, 0) =/= 0.U)
    val invalid = ~CheckValidRamAddress(paddr)
    val pageFault = ~entry.X || (privilege === S_LEVEL && entry.U)
    exception.valid := misaligned || invalid || pageFault
    exception.code := MuxCase(0.U, 
      Seq(
        misaligned -> EC_IA_MISALIGNED,
        invalid -> EC_IA_FAULT,
        pageFault -> EC_FETCH_PF
      )
    )
  }

  def checkAddressFormat(
      paddr: UInt,
      fetch: Bool,
      load: Bool,
      store: Bool
  ): Exception = {
    val exception = WireInit(0.U.asTypeOf(new Exception))
    val misaligned = (paddr(1, 0) =/= 0.U)
    val invalid = invalidAddr(paddr)
    exception.valid := misaligned || invalid
    exception.code := MuxCase(
      0.U,
      Seq(
        misaligned -> MuxCase(
          0.U,
          Seq(
            fetch -> EC_IA_MISALIGNED,
            load -> EC_LA_MISALIGNED,
            store -> EC_SAIA_MISALIGNED
          )
        ),
        invalid -> MuxCase(
          0.U,
          Seq(
            fetch -> EC_IA_FAULT,
            load -> EC_LA_FAULT,
            store -> EC_SAIA_FAULT
          )
        )
      )
    )
    exception
  }

  def checkPagePermissionLevel1(
      pte: PageTableEntry,
      privilege: UInt,
      fetch: Bool,
      load: Bool,
      store: Bool,
      sum: Bool
  ): Exception = {
    val exception = WireInit(0.U.asTypeOf(new Exception))
    val pageFault = Wire(Bool())
    pageFault := (
      (pte.V === false.B) ||
        (privilege === U_LEVEL && pte.U === false.B)
        // (privilege === S_LEVEL && pte.U === true.B && !sum)
    )
    exception.valid := pageFault
    exception.code := MuxCase(
      0.U,
      Seq(
        pageFault -> MuxCase(
          0.U,
          Seq(
            fetch -> EC_FETCH_PF,
            load -> EC_LOAD_PF,
            store -> EC_STORE_PF
          )
        )
      )
    )
    exception
  }

  def checkPagePermissionLevel2(
      pte: PageTableEntry,
      privilege: UInt,
      fetch: Bool,
      load: Bool,
      store: Bool,
      sum: Bool,
      offset: UInt
  ): Exception = {
    val exc1 = WireInit(0.U.asTypeOf(new Exception))
    val exc2 = WireInit(0.U.asTypeOf(new Exception))
    val paddr = Cat(pte.ppn1, pte.ppn0, offset)(31, 0)
    exc1 := checkAddressFormat(paddr, fetch, load, store)
    val pageFault = Wire(Bool())
    pageFault := (
      (pte.V === false.B) ||
        (fetch && pte.X === false.B) ||
        (load && pte.R === false.B) ||
        (store && pte.W === false.B) ||
        (privilege === U_LEVEL && pte.U === false.B) ||
        (privilege === S_LEVEL && pte.X === true.B)
        // ((privilege === S_LEVEL) & (pte.U === true.B) & (!sum))
    )
    exc2.valid := pageFault
    exc2.code := MuxCase(
      0.U,
      Seq(
        pageFault -> MuxCase(
          0.U,
          Seq(
            fetch -> EC_FETCH_PF,
            load -> EC_LOAD_PF,
            store -> EC_STORE_PF
          )
        )
      )
    )
    Mux(exc1.valid, exc1, exc2)
  }
}
