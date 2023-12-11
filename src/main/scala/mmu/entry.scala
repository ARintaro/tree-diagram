package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._

class VirtualAddress extends Bundle {
    val vpn1 = UInt(VPN1_WIDTH)
    val vpn0 = UInt(VPN0_WIDTH)
    val offset = UInt(OFFSET_WIDTH)
}

class PhysicalAddress extends Bundle {
    val ppn1 = UInt(PPN1_WIDTH)
    val ppn0 = UInt(PPN0_WIDTH)
    val offset = UInt(OFFSET_WIDTH)
}

class PageTableEntry extends Bundle {
    val ppn1 = UInt(PPN1_WIDTH)
    val ppn0 = UInt(PPN0_WIDTH)
    val _p_0 = UInt(5.W) // no RSW, D, A, G
    val U = Bool() // user accessible
    val X = Bool() // executable
    val W = Bool() // writable
    val R = Bool() // readable
    val V = Bool() // valid
}

class MemoryMappingCommit extends Bundle {
    val valid = Bool()
    val vaddr = UInt(BusConfig.ADDR_WIDTH)
    val fetch = Bool()
    val load = Bool()
    val store = Bool()
}

class MemoryMappingResult extends Bundle {
    val done = Bool()
    val paddr = UInt(BusConfig.ADDR_WIDTH)
    val exception = Bool()
    val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)
}

