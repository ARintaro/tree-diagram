package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._
import AddressException._

class InstructionMemoryManagementUnit extends Module {
    val io = IO(new Bundle {
        val vaddr = Input(UInt(32.W))
        val paddr = Valid(UInt(32.W))
        val vaddr_out = Output(UInt(32.W))
        val error = Output(new Error)
        val bus = BusMasterInterface()
    })

    val ctrlIO = IO(new Bundle {
        val flush = Input(Bool())
        val clearTLB = Input(Bool())
    })

    val satp = WireInit(0.U.asTypeOf(new csr_satp_t))
    BoringUtils.addSink(satp, "satp")
    val privilege = WireInit(0.U(2.W))
    BoringUtils.addSink(privilege, "globalPrivilegeLevel")
    val sum = Bool()
    // BoringUtils.addSink(sum, "statusSum")

    val paddr_bits = RegInit(0.U(32.W))
    val paddr_valid = RegInit(false.B)
    io.paddr.valid := paddr_valid
    io.paddr.bits := paddr_bits
    val error = RegInit(0.U.asTypeOf(new Error))
    io.error := error
    io.bus.master_turn_off()
    io.bus.dataBytesSelect := "b1111".U
    io.bus.dataMode := false.B

    val last_vaddr = RegInit(0.U.asTypeOf(new VirtualAddress))
    io.vaddr_out := last_vaddr.asUInt

    // inner tlb
    val tlb = Module(new TranslationLookasideBuffer)
    tlb.io.searchReq.submit := false.B
    tlb.io.searchReq.vpn1 := DontCare
    tlb.io.searchReq.vpn0 := DontCare
    tlb.io.insertReq.submit := false.B
    tlb.io.insertReq.entry := DontCare
    tlb.ctrlIO.clear := ctrlIO.clearTLB

    val List(tlb_S, pt1_S, pt2_S) = Enum(3)
    val state = RegInit(tlb_S)

    val acked = RegInit(false.B)
    val midPTE = RegInit(0.U.asTypeOf(new PageTableEntry))
    val leafPTE = RegInit(0.U.asTypeOf(new PageTableEntry))

    switch(state){
        is(tlb_S){
            last_vaddr := io.vaddr.asTypeOf(new VirtualAddress)
            when(acked){
                error := checkPagePermissionLevel2(leafPTE, privilege, true.B, false.B, false.B, sum, last_vaddr.offset)
                paddr_valid := true.B
                paddr_bits := Cat(leafPTE.ppn1, leafPTE.ppn0, last_vaddr.offset)(31, 0)
                val newEntry = WireInit(0.U.asTypeOf(new TLBEntry))
                newEntry.vpn1 := last_vaddr.vpn1
                newEntry.vpn0 := last_vaddr.vpn0
                newEntry.pte := leafPTE
                tlb.io.insertReq.entry := newEntry
                acked := false.B
            }
            when(satp.mode === 0.U){
                paddr_bits := io.vaddr
                paddr_valid := true.B
                error := checkAddressFormat(io.vaddr, true.B, false.B, false.B)
            }.otherwise{
                tlb.io.searchReq.vpn1 := io.vaddr.asTypeOf(new VirtualAddress).vpn1
                tlb.io.searchReq.vpn0 := io.vaddr.asTypeOf(new VirtualAddress).vpn0
                tlb.io.searchReq.submit := true.B
                state := pt1_S
            }
        }
        is(pt1_S){
            when(tlb.io.result.hit){
                paddr_valid := true.B
                paddr_bits := Cat(tlb.io.result.pte.ppn1, tlb.io.result.pte.ppn0, last_vaddr.offset)(31, 0)
                state := tlb_S
            }.otherwise{
                io.bus.addr := Cat(satp.ppn, last_vaddr.vpn1, 0.U(2.W))(31, 0)
                io.bus.stb := true.B
                when(io.bus.ack){
                    midPTE := io.bus.dataRead.asTypeOf(new PageTableEntry)
                    state := pt2_S
                }
            }
        }
        is(pt2_S){
            io.error := checkPagePermissionLevel1(midPTE, privilege, true.B, false.B, false.B, sum)
            when(io.error.en){
                state := tlb_S
                paddr_valid := true.B
            }.otherwise{
                io.bus.addr := Cat(midPTE.ppn1, last_vaddr.vpn0, last_vaddr.offset)(31, 0)
                io.bus.stb := true.B
                when(io.bus.ack){
                    leafPTE := io.bus.dataRead.asTypeOf(new PageTableEntry)
                    state := tlb_S
                    acked := true.B
                }
            }
        }
    }

    when(ctrlIO.flush){
        state := tlb_S
        midPTE := 0.U.asTypeOf(new PageTableEntry)
        leafPTE := 0.U.asTypeOf(new PageTableEntry)
        acked := false.B
    }
}
