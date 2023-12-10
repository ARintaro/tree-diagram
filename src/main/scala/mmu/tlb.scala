package core

import chisel3._
import chisel3.util._

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._

object MemoryManagementConstants {
    val TLB_ENTRY_NUM = 64
    val TLB_ENTRY_WIDTH = log2Ceil(TLB_ENTRY_NUM)

    val VPN_WIDTH = 20.W
    val PPN_WIDTH = 22.W
    val VPN1_WIDTH = 10.W
    val VPN0_WIDTH = 10.W
    val PPN1_WIDTH = 12.W
    val PPN0_WIDTH = 10.W
    val OFFSET_WIDTH = 12.W
}

class TLBSearchRequest extends Bundle {
    val vpn1 = UInt(VPN1_WIDTH)
    val vpn0 = UInt(VPN0_WIDTH)
    val submit = Bool()
}

class TLBInsertRequest extends Bundle {
    val entry = new TLBEntry
    val submit = Bool()
}

class TLBEntry extends Bundle {
    val vpn1 = UInt(VPN1_WIDTH) // tag
    val vpn0 = UInt(VPN0_WIDTH) // tag
    val pte = new PageTableEntry
}

class TLBSearchResponse extends Bundle {
    val hit = Bool()
    val pte = new PageTableEntry
}

class TranslationLookasideBuffer extends Module {
    val io = IO(new Bundle {
        val searchReq = Input(new TLBSearchRequest)
        val insertReq = Input(new TLBInsertRequest)
        val searchResp = Output(new TLBSearchResponse)
    })

    val ctrlIO = IO(new Bundle {
        val clear = Input(Bool())
    })

    val entries = RegInit(VecInit(Seq.fill(TLB_ENTRY_NUM)(0.U.asTypeOf(new TLBEntry))))

    val outBuffer = RegInit(0.U.asTypeOf(new PageTableEntry))
    io.searchResp.pte := outBuffer

    // random
    val random = RegInit(0.U(TLB_ENTRY_WIDTH.W))
    random := random + 1.U

    val validBits = RegInit(0.U(TLB_ENTRY_NUM.W))

    // situation 1: search
    when(io.searchReq.submit){
        entries.zipWithIndex.foreach{ case (entry, i) => {
            when(entry.vpn1 === io.searchReq.vpn1 && entry.vpn0 === io.searchReq.vpn0 && validBits(i) === 1.U){
                outBuffer := entry.pte
                io.searchResp.hit := true.B
            }
        }}
    }
    
    // situation 2: insert
    when(io.insertReq.submit){
        val vaccant = validBits.asBools.map(!_).reduce(_||_)
        val firstAvailable = PriorityEncoder(validBits.asBools.map(!_))
        when (vaccant){
            entries(firstAvailable) := io.insertReq.entry
            validBits(firstAvailable) := 1.U
        }.otherwise{
            entries(random) := io.insertReq.entry
        }
    }

    // clear
    when(ctrlIO.clear){
        validBits := 0.U
    }
}

