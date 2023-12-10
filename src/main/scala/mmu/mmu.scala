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

class MemoryManagementUnit extends Module {
    val io = IO(new Bundle {
        val satp = Input(new csr_satp_t)
        val in = Input(new MemoryMappingCommit)
        val out = Output(new MemoryMappingResult)
        val bus = BusMasterInterface()
    })

    val ctrlIO = IO(new Bundle {
        val clearTLB = Input(Bool())
    })

    val vaddr = WireInit(io.in.vaddr.asTypeOf(new VirtualAddress))
    val privilege = WireInit(0.U(2.W))
    BoringUtils.addSink(privilege, "globalPrivilegeLevel")
    val sum = Bool()
    BoringUtils.addSink(sum, "statusSum")
    val vpn1 = RegInit(0.U(VPN1_WIDTH))
    vpn1 := vaddr.vpn1
    val vpn0 = RegInit(0.U(VPN0_WIDTH))
    vpn0 := vaddr.vpn0

    // init
    ctrlIO.clearTLB := false.B
    io.bus.master_turn_off()
    io.bus.dataBytesSelect := "b1111".U
    io.out.done := false.B
    io.out.paddr := 0.U
    io.out.exception := false.B
    io.out.exceptionCode := 0.U
    val outPTE = WireInit(0.U.asTypeOf(new PageTableEntry))
    io.out.paddr := Cat(outPTE.ppn1, outPTE.ppn0, vaddr.offset)(31, 0)
    // val middlePTE = RegInit(0.U.asTypeOf(new PageTableEntry))

    /*============= page fault judgement begin ============= */
    def invalidAddr(addr: UInt): Bool = {
        val valid = WireInit(false.B)
        when ((addr & BASE_RAM_MASK.U) === BASE_RAM_START.U) {
            valid := true.B
        }.elsewhen ((addr & EXT_RAM_MASK.U) === EXT_RAM_START.U) {
            valid := true.B
        }.elsewhen ((addr & UART_MASK.U) === UART_START.U) {
            valid := true.B
        }.elsewhen ((addr & TIMER_MASK.U) === TIMER_START.U) {
            valid := true.B
        }.otherwise (valid := false.B)
        // FIXME: 之后还要补充对其他外设的判断
        !valid
    }
    assert(PopCount(Seq(io.in.fetch, io.in.load, io.in.store)) === 1.U, "MemoryMappingCommit Error")
    when(vaddr.offset(1, 0) =/= 0.U){
        io.out.exception := true.B
        io.out.exceptionCode := MuxCase(EC_IA_MISALIGNED, Seq(
            io.in.fetch -> EC_IA_MISALIGNED,
            io.in.load -> EC_LA_MISALIGNED,
            io.in.store -> EC_SAIA_MISALIGNED
        ))
    }
    .elsewhen(invalidAddr(io.out.paddr)){
        io.out.exception := true.B
        io.out.exceptionCode := MuxCase(EC_IA_FAULT, Seq(
            io.in.fetch -> EC_IA_FAULT,
            io.in.load -> EC_LA_FAULT,
            io.in.store -> EC_SAIA_FAULT
        ))
    }
    .elsewhen(~outPTE.V){
        io.out.exception := true.B
        io.out.exceptionCode := MuxCase(EC_FETCH_PF, Seq(
            io.in.fetch -> EC_FETCH_PF,
            io.in.load -> EC_LOAD_PF,
            io.in.store -> EC_STORE_PF
        ))
    }
    .elsewhen(io.in.fetch && ~outPTE.X
        || io.in.load && ~outPTE.R
        || io.in.store && ~outPTE.W
        || privilege === U_LEVEL && ~outPTE.U
        || privilege === S_LEVEL && outPTE.U && ~sum
        || privilege === S_LEVEL && outPTE.X){
        io.out.exception := true.B
        io.out.exceptionCode := MuxCase(0.U, Seq(
            io.in.fetch -> EC_FETCH_PF,
            io.in.load -> EC_LOAD_PF,
            io.in.store -> EC_STORE_PF
        ))
    }
    /*============== page fault judgement end ============== */

    // tlb
    val tlb = Module(new TranslationLookasideBuffer)
    tlb.ctrlIO.clear := ctrlIO.clearTLB
    tlb.io.searchReq.vpn1 := vaddr.vpn1
    tlb.io.searchReq.vpn0 := vaddr.vpn0
    tlb.io.searchReq.submit := false.B
    tlb.io.insertReq.submit := false.B

    /*
        MMU:
        1. search in TLB and wait for result; if hit then return
        2. search in page table (level 1)
        3. search in page table (level 2)
        4. if hit then insert into TLB and return; else page fault
    */

    val List(idle_s, tlb_s, pt1_s, pt2_s) = Enum(4)
    val state = RegInit(idle_s)

    switch(state){
        is(idle_s){
            when(io.in.valid){
                when(~io.satp.mode){
                    io.out.done := true.B
                    io.out.paddr := RegNext(io.in.vaddr)
                    io.out.exception := RegNext(false.B)
                }.otherwise{
                    tlb.io.searchReq.submit := true.B
                    io.out.done := tlb.io.searchResp.hit
                    outPTE := tlb.io.searchResp.pte
                    when(!tlb.io.searchResp.hit){
                        state := tlb_s
                    }
                }
            }
        }
        is(tlb_s){
            io.bus.addr := (io.satp.ppn << 12.U + vaddr.vpn1 << 2.U)(31, 0)
            when(io.bus.ack){
                state := pt1_s
            }
        }
        is(pt1_s){
            val midPTE = io.bus.dataRead.asTypeOf(new PageTableEntry)
            when(~midPTE.V){
                io.out.done := true.B
                io.out.exception := RegNext(true.B)
                io.out.exceptionCode := RegNext(io.out.exceptionCode)
                state := idle_s
            }.otherwise{
                io.bus.addr := Cat(outPTE.ppn1(9, 0), outPTE.ppn0(9, 0), 0.U(12.W)) + vaddr.vpn0 << 2.U
                io.out.done := io.bus.ack
                when(io.bus.ack){
                    state := pt2_s
                }
            }
        }
        is(pt2_s){
            outPTE := io.bus.dataRead.asTypeOf(new PageTableEntry)
            val newEntry = WireInit(0.U.asTypeOf(new TLBEntry))
            newEntry.pte := outPTE
            newEntry.vpn1 := vpn1
            newEntry.vpn0 := vpn0
            tlb.io.insertReq.entry := newEntry
            tlb.io.insertReq.submit := ~io.out.exception
            when(io.in.valid){
                when(~io.satp.mode){
                    io.out.done := true.B
                    io.out.paddr := RegNext(io.in.vaddr)
                    io.out.exception := RegNext(false.B)
                    state := idle_s
                }.otherwise{
                    tlb.io.searchReq.submit := true.B
                    io.out.done := tlb.io.searchResp.hit
                    outPTE := tlb.io.searchResp.pte
                    when(!tlb.io.searchResp.hit){
                        state := tlb_s
                    }.otherwise{
                        state := idle_s
                    }
                }
            }.otherwise{
                state := idle_s
            }
        }
    }
}