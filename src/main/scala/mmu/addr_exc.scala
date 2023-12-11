package core

import chisel3._
import chisel3.util._

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._

class Error extends Bundle {
    val en = Bool()
    val code = UInt(InsConfig.EXCEPTION_WIDTH)
}

object AddressException {
    def invalidAddr(paddr: UInt): Bool = {
        val valid = WireInit(false.B)
        when ((paddr & BASE_RAM_MASK.U) === BASE_RAM_START.U) {
            valid := true.B
        }.elsewhen ((paddr & EXT_RAM_MASK.U) === EXT_RAM_START.U) {
            valid := true.B
        }.elsewhen ((paddr & UART_MASK.U) === UART_START.U) {
            valid := true.B
        }.elsewhen ((paddr & TIMER_MASK.U) === TIMER_START.U) {
            valid := true.B
        }.otherwise (valid := false.B)
        // FIXME: 之后还要补充对其他外设的判断
        !valid
    }

    def checkAddressFormat(paddr: UInt, fetch: Bool, load: Bool, store: Bool): Error = {
        val error = WireInit(0.U.asTypeOf(new Error))
        val misaligned = (paddr(1, 0) =/= 0.U)
        val invalid = invalidAddr(paddr)
        error.en := misaligned || invalid
        error.code := MuxCase(0.U, Seq(
            misaligned -> MuxCase(0.U, Seq(
                fetch -> EC_IA_MISALIGNED,
                load -> EC_LA_MISALIGNED,
                store -> EC_SAIA_MISALIGNED
            )),
            invalid -> MuxCase(0.U, Seq(
                fetch -> EC_IA_FAULT,
                load -> EC_LA_FAULT,
                store -> EC_SAIA_FAULT
            ))
        ))
        error
    }

    def checkPagePermissionLevel1(pte: PageTableEntry, privilege: UInt,
    fetch: Bool, load: Bool, store: Bool, sum: Bool): Error = {
        val error = WireInit(0.U.asTypeOf(new Error))
        val pageFault = Wire(Bool())
        pageFault := (
            (pte.V === false.B) ||
            (privilege === U_LEVEL && pte.U === false.B)
            // (privilege === S_LEVEL && pte.U === true.B && !sum)
        )
        error.en := pageFault
        error.code := MuxCase(0.U, Seq(
            pageFault -> MuxCase(0.U, Seq(
                fetch -> EC_FETCH_PF,
                load -> EC_LOAD_PF,
                store -> EC_STORE_PF
            ))
        ))
        error
    }

    def checkPagePermissionLevel2(pte: PageTableEntry, privilege: UInt, 
    fetch: Bool, load: Bool, store: Bool, sum: Bool, offset: UInt): Error = {
        val error1 = WireInit(0.U.asTypeOf(new Error))
        val error2 = WireInit(0.U.asTypeOf(new Error))
        val paddr = Cat(pte.ppn1, pte.ppn0, offset)(31, 0)
        error1 := checkAddressFormat(paddr, fetch, load, store)
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
        error2.en := pageFault
        error2.code := MuxCase(0.U, Seq(
            pageFault -> MuxCase(0.U, Seq(
                fetch -> EC_FETCH_PF,
                load -> EC_LOAD_PF,
                store -> EC_STORE_PF
            ))
        ))
        Mux(error1.en, error1, error2)
    }
}