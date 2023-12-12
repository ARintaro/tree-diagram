package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

object PrivilegeLevel {
  val U_LEVEL = 0.U(2.W)
  val S_LEVEL = 1.U(2.W)
  val M_LEVEL = 3.U(2.W)
}
import PrivilegeLevel._

object CsrConstants {
  val CSR_ADDR_WIDTH = 12.W

  val CSR_MSTATUS_ADDR  = 0x300.U(12.W)
  val CSR_MCAUSE_ADDR   = 0x342.U(12.W)
  val CSR_MTVEC_ADDR    = 0x305.U(12.W)
  val CSR_MEPC_ADDR     = 0x341.U(12.W)
  val CSR_MIE_ADDR      = 0x304.U(12.W)
  val CSR_MIP_ADDR      = 0x344.U(12.W)
  val CSR_MSCRATCH_ADDR = 0x340.U(12.W)
  val CSR_MTVAL_ADDR    = 0x343.U(12.W)
  val CSR_MEDELEG_ADDR  = 0x302.U(12.W)
  val CSR_MIDELEG_ADDR  = 0x303.U(12.W)
  val CSR_MHARTID_ADDR  = 0xf14.U(12.W)

  val CSR_SSTATUS_ADDR  = 0x100.U(12.W)
  val CSR_SCAUSE_ADDR   = 0x142.U(12.W)
  val CSR_STVEC_ADDR    = 0x105.U(12.W)
  val CSR_SEPC_ADDR     = 0x141.U(12.W)
  val CSR_SIE_ADDR      = 0x104.U(12.W)
  val CSR_SIP_ADDR      = 0x144.U(12.W)
  val CSR_SSCRATCH_ADDR = 0x140.U(12.W)
  val CSR_STVAL_ADDR    = 0x143.U(12.W)
  val CSR_SATP_ADDR     = 0x180.U(12.W)

  val CSR_TIME_ADDR     = 0xc01.U(12.W)
  val CSR_TIMEH_ADDR    = 0xc81.U(12.W)

  val CSR_MTIME_MEM_ADDR    = 0x200bff8.U(32.W)
  val CSR_MTIMECMP_MEM_ADDR = 0x2004000.U(32.W)  
}

class csr_status_t extends Bundle {
  val sd = Bool()
  val _p_0 = UInt(8.W)
  val tsr = Bool()
  val tw = Bool()
  val tvm = Bool()
  val mxr = Bool()
  val sum = Bool()
  val mprv = Bool()
  val xs = UInt(2.W)
  val fs = UInt(2.W)
  val mpp = UInt(2.W)
  val vs = UInt(2.W)
  val spp = Bool()
  val mpie = Bool()
  val ube = Bool()
  val spie = Bool()
  val upie = Bool()
  val mie = Bool()
  val _p_2 = Bool()
  val sie = Bool()
  val uie = Bool()

  def reg(privilege: UInt): UInt = {
    val register = Wire(UInt(32.W))
    register := MuxLookup(privilege, 0.U)(Seq(
      M_LEVEL -> Cat(sd, 0.U(8.W), tsr, tw, tvm, mxr, sum, mprv, xs, fs, mpp, vs, spp, mpie, ube, spie, 0.U, mie, 0.U, sie, 0.U),
      S_LEVEL -> Cat(sd, 0.U(11.W)             , mxr, sum, 0.U , xs, fs, 0.U(2.W), vs, spp, 0.U , ube, spie, 0.U(3.W)     , sie, 0.U)
    ))
    register
  }
}

class csr_ie_t extends Bundle {
  val _p_0 = UInt(16.W)
  val _p_1 = UInt(4.W)
  val meie = Bool()
  val _p_2 = Bool()
  val seie = Bool()
  val _p_3 = Bool()
  val mtie = Bool()
  val _p_4 = Bool()
  val stie = Bool()
  val _p_5 = Bool()
  val msie = Bool()
  val _p_6 = Bool()
  val ssie = Bool()
  val _p_7 = Bool()
  
  def reg(privilege: UInt): UInt = {
    val register = Wire(UInt(32.W))
    register := MuxLookup(privilege, 0.U)(Seq(
      M_LEVEL -> Cat(0.U(16.W), 0.U(4.W), meie, 0.U, seie, 0.U, mtie, 0.U, stie, 0.U, msie, 0.U, ssie, 0.U),
      S_LEVEL -> Cat(0.U(16.W), 0.U(6.W)           , seie, 0.U(3.W)      , stie, 0.U(3.W)      , ssie, 0.U)
    ))
    register
  }
}

class csr_ip_t extends Bundle {
  val _p_0 = UInt(16.W)
  val _p_1 = UInt(4.W)
  val meip = Bool()
  val _p_2 = Bool()
  val seip = Bool()
  val _p_3 = Bool()
  val mtip = Bool()
  val _p_4 = Bool()
  val stip = Bool()
  val _p_5 = Bool()
  val msip = Bool()
  val _p_6 = Bool()
  val ssip = Bool()
  val _p_7 = Bool()

  def reg(privilege: UInt): UInt = {
    val register = Wire(UInt(32.W))
    register := MuxLookup(privilege, 0.U)(Seq(
      M_LEVEL -> Cat(0.U(16.W), 0.U(4.W), meip, 0.U, seip, 0.U, mtip, 0.U, stip, 0.U, msip, 0.U, ssip, 0.U),
      S_LEVEL -> Cat(0.U(16.W), 0.U(6.W)           , seip, 0.U(3.W)      , stip, 0.U(3.W)      , ssip, 0.U)
    ))
    register
  }
}

class csr_cause_t extends Bundle {
  val interrupt = Bool()
  val exceptionCode = UInt(31.W)

  def reg: UInt = {
    val register = Wire(UInt(32.W))
    register := Cat(interrupt, exceptionCode)
    register
  }
}

class csr_tvec_t extends Bundle {
  val base = UInt(30.W)
  val mode = UInt(2.W)

  def reg: UInt = {
    val register = Wire(UInt(32.W))
    register := Cat(base, mode)
    register
  }
}

class csr_satp_t extends Bundle {
  val mode = Bool()
  val asid = UInt(9.W)
  val ppn = UInt(22.W)

  def reg: UInt = {
    val register = Wire(UInt(32.W))
    register := Cat(mode, asid, ppn)
    register
  }
}
