package core

import chisel3._
import chisel3.util._

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import AddressException._

object MemoryManagementConstants {
  val TLB_ENTRY_NUM = 4
  val TLB_ENTRY_WIDTH = log2Ceil(TLB_ENTRY_NUM)

  val ASID_WIDTH = 9.W
  val VPN_WIDTH = 20.W
  val PPN_WIDTH = 22.W
  val VPN1_WIDTH = 10.W
  val VPN0_WIDTH = 10.W
  val PPN1_WIDTH = 12.W
  val PPN0_WIDTH = 10.W
  val OFFSET_WIDTH = 12.W
}


class TLBInsertRequest extends Bundle {
  val tag = new TLBTag
  val entry = new PageTableEntry
  val submit = Bool()
}

class TLBTag extends Bundle {
  val asid = UInt(ASID_WIDTH)
  val vpn1 = UInt(VPN1_WIDTH) 
  val vpn0 = UInt(VPN0_WIDTH) 
}

class TLBSearchResponse extends Bundle {
  val hit = Bool()
  val pte = new PageTableEntry
}

class TranslationLookasideBuffer extends Module {
  val io = IO(new Bundle {
    val insert = Input(new TLBInsertRequest)
    
    val search = Input(new TLBTag)
    val result = Output(new TLBSearchResponse)
  })

  val ctrlIO = IO(new Bundle {
    val clear = Input(Bool())
  })

  val entries = RegInit(VecInit(Seq.fill(TLB_ENTRY_NUM)(0.U.asTypeOf(new PageTableEntry))))
  val tags = RegInit(VecInit(Seq.fill(TLB_ENTRY_NUM)(0.U.asTypeOf(new TLBTag))))
  val validBits = RegInit(0.U(TLB_ENTRY_NUM.W))

  val outBuffer = RegInit(0.U.asTypeOf(new TLBSearchResponse))

  io.result := outBuffer

  val random = RegInit(0.U(TLB_ENTRY_WIDTH.W))
  random := random + 1.U

  val hits = VecInit(tags.map(x => x.vpn1 === io.search.vpn1 && x.vpn0 === io.search.vpn0 && x.asid === io.search.asid)).asUInt & validBits

  outBuffer.hit := hits.orR
  outBuffer.pte := entries(OHToUInt(hits))

  assert(PopCount(hits) <= 1.U)
  
  when(io.insert.submit) {
    val invalids = validBits.asBools.map(!_)
    val firstAvailable = PriorityEncoder(invalids)
    when(invalids.reduce(_ || _)) {
      entries(firstAvailable) := io.insert.entry
      tags(firstAvailable) := io.insert.tag
      validBits := validBits.bitSet(firstAvailable, true.B)
    }.otherwise {
      entries(random) := io.insert.entry
      tags(random) := io.insert.tag
    }
  }

  when(ctrlIO.clear) {
    validBits := 0.U
  }
}
