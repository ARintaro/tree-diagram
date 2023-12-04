package core

import chisel3._
import chisel3.util._

import BusConfig._

trait PageConstants {
    val PAGE_LENGTH = 4096
    val PAGE_WIDTH = 12
}

class PhysicalAddress extends Bundle{
    val ppn1 = UInt(12.W)
    val ppn0 = UInt(10.W)
    val offset = UInt(12.W)
}

class VirtualAddress extends Bundle {
    val vpn1 = UInt(10.W)
    val vpn0 = UInt(10.W)
    val offset = UInt(12.W)
}

class PageTableEntry extends Bundle {
    val ppn1 = UInt(12.W)
    val ppn0 = UInt(10.W)
    val rsw = UInt(2.W)
    val D = Bool()
    val A = Bool()
    val G = Bool()
    val U = Bool()
    val X = Bool()
    val W = Bool()
    val R = Bool()
    val V = Bool()
}

class MemoryManagementUnit extends Module {
    val io = IO(new Bundle {
        
    })
}

