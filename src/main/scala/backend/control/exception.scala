package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import CsrConstants._
import PrivilegeLevel._
import InsConfig.ExceptionCode._

class ExceptionRequest extends Bundle {
    val valid = Bool()
    val exceptionPC = UInt(32.W)
    val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)
    val exceptionValue = UInt(32.W)
}

class CsrInstruction 
  extends Bundle
  with InstructionConstants
  with IssueInstruction {
    
    val currentPC = UInt(32.W)
    val csrType = UInt(CSR_WIDTH)
    val csrAddr = UInt(CSR_ADDR_WIDTH)
    val prs = UInt(BackendConfig.pregIdxWidth)
    val prd = UInt(BackendConfig.pregIdxWidth)
    val uimm = UInt(32.W)
    val readReg = Bool()
    val writeReg = Bool()

    override def checkReady(busy: UInt): Bool = {
      return !busy(prs)
    }
  }


class ExceptionUnit extends Module with InstructionConstants {
    val io = IO(new Bundle {
        val request = Input(new ExceptionRequest)
        val in = Flipped(Decoupled(new CsrInstruction))
        val regRead = new RegisterReadRequest
        val regWrite = new RegisterWriteRequest

        val redirect = Valid(UInt(BusConfig.ADDR_WIDTH))
        val flush = Output(Bool())
    })

    /* ================global privilege level ================ */
    val globalPrivilegeLevel = RegInit(M_LEVEL)

    /* =================== CSR =================== */
    val status = RegInit(0.U.asTypeOf(new csr_status_t)) // mstatus, sstatus
    val ie = RegInit(0.U.asTypeOf(new csr_ie_t)) // mie, sie
    val ip = RegInit(0.U.asTypeOf(new csr_ip_t)) // mip, sip

    val mcause = RegInit(0.U.asTypeOf(new csr_cause_t)) // mcause
    val scause = RegInit(0.U.asTypeOf(new csr_cause_t)) // scause
    val mepc_reg = RegInit(0.U(32.W)) // mepc
    val sepc_reg = RegInit(0.U(32.W)) // sepc
    val mtval_reg = RegInit(0.U(32.W)) // mtval
    val stval_reg = RegInit(0.U(32.W)) // stval
    val mscratch_reg = RegInit(0.U(32.W)) // mscratch
    val sscratch_reg = RegInit(0.U(32.W)) // sscratch
    val mtvec = RegInit(0.U.asTypeOf(new csr_tvec_t)) // mtvec
    val stvec = RegInit(0.U.asTypeOf(new csr_tvec_t)) // stvec
    
    val medeleg_reg = RegInit(0.U(32.W)) // medeleg
    val mideleg_reg = RegInit(0.U(32.W)) // mideleg
    val mhartid_reg = RegInit(0.U(32.W)) // mhartid

    /* ================interrupt logic ================ */
    val meiOccur = ie.meie & ip.meip
    val seiOccur = ie.seie & ip.seip
    val mtiOccur = ie.mtie & ip.mtip
    val stiOccur = ie.stie & ip.stip
    val msiOccur = ie.msie & ip.msip
    val ssiOccur = ie.ssie & ip.ssip
    val mInterrupt = meiOccur | mtiOccur | msiOccur
    val sInterrupt = seiOccur | stiOccur | ssiOccur
    val interruptOccur = MuxCase(false.B, Seq(
        (globalPrivilegeLevel === M_LEVEL) -> (mInterrupt & status.mie),
        (globalPrivilegeLevel === S_LEVEL) -> (mInterrupt | (sInterrupt & status.sie)),
        (globalPrivilegeLevel === U_LEVEL) -> (mInterrupt | sInterrupt)
    ))
    val interruptCode = Wire(UInt(InsConfig.EXCEPTION_WIDTH))
    interruptCode := MuxCase(0.U, Seq(
        meiOccur -> IT_M_EXT_INT,
        msiOccur -> IT_M_SOFT_INT,
        mtiOccur -> IT_M_TIMER_INT,
        seiOccur -> IT_S_EXT_INT,
        stiOccur -> IT_S_TIMER_INT,
        ssiOccur -> IT_S_SOFT_INT
    ))

    /* ================check privilege ================ */
    val csrReadOnly = io.in.bits.csrAddr(11, 10) === 3.U
    val csrPrivilegeLevel = io.in.bits.csrAddr(9, 8)
    val canRead = csrPrivilegeLevel <= globalPrivilegeLevel
    val canWrite = csrPrivilegeLevel <= globalPrivilegeLevel && !csrReadOnly

    /* ================ Read logic ================ */
    val csrReadData = MuxLookup(io.in.bits.csrAddr, 0.U(32.W))(Seq(
        CSR_MSTATUS_ADDR -> status.reg(M_LEVEL),
        CSR_MTVEC_ADDR -> mtvec.asUInt,
        CSR_MIP_ADDR -> ip.reg(M_LEVEL),
        CSR_MIE_ADDR -> ie.reg(M_LEVEL),
        CSR_MSCRATCH_ADDR -> mscratch_reg,
        CSR_MEPC_ADDR -> mepc_reg,
        CSR_MCAUSE_ADDR -> mcause.asUInt,
        CSR_MHARTID_ADDR -> mhartid_reg,
        CSR_MIDELEG_ADDR -> mideleg_reg,
        CSR_MEDELEG_ADDR -> medeleg_reg,
        CSR_MTVAL_ADDR -> mtval_reg,

        CSR_SSTATUS_ADDR -> status.reg(S_LEVEL),
        CSR_STVEC_ADDR -> stvec.asUInt,
        CSR_SIP_ADDR -> ip.reg(S_LEVEL),
        CSR_SIE_ADDR -> ie.reg(S_LEVEL),
        CSR_SSCRATCH_ADDR -> sscratch_reg,
        CSR_SEPC_ADDR -> sepc_reg,
        CSR_SCAUSE_ADDR -> scause.asUInt,
        CSR_STVAL_ADDR -> stval_reg
    ))
    when(io.in.valid && io.in.bits.writeReg && canRead) { // 注意这里writeReg指的是写入物理寄存器，而canRead指的是csr寄存器可读
        io.regWrite.id := io.in.bits.prd
        io.regWrite.value := csrReadData
    }

    /* ==========trap deleg logic ========== */
    val delegException = Wire(Bool())
    delegException := MuxCase(medeleg_reg(io.request.exceptionCode), Seq(
        (globalPrivilegeLevel === M_LEVEL) -> false.B,
        interruptOccur -> mideleg_reg(interruptCode),
    ))

    /* ============Redirect & flush logic ============ */
    val nextPC = Wire(UInt(32.W))
    val nextPrivilegeLevel = Wire(UInt(2.W))
    val returnFromException = (io.in.bits.csrType === MRET) || (io.in.bits.csrType === SRET)
    val intoException = io.request.valid
    when(intoException){
        nextPC := Mux(delegException,
            Mux(stvec.mode.asBool,
            Cat(stvec.base, Fill(2, 0.U)),
            Cat(stvec.base, Fill(2, 0.U)) + (io.request.exceptionCode << 2.U)),
            Mux(mtvec.mode.asBool,
            Cat(mtvec.base, Fill(2, 0.U)),
            Cat(mtvec.base, Fill(2, 0.U)) + (io.request.exceptionCode << 2.U))
        )
        nextPrivilegeLevel := Mux(delegException,
            S_LEVEL,
            M_LEVEL
        )
    }.elsewhen(returnFromException){
        nextPC := Mux(globalPrivilegeLevel === M_LEVEL,
            mepc_reg,
            sepc_reg
        )
        nextPrivilegeLevel := Mux(globalPrivilegeLevel === M_LEVEL,
            status.mpp,
            status.spp
        )
    }.otherwise{
        nextPC := io.in.bits.currentPC + 4.U
        nextPrivilegeLevel := globalPrivilegeLevel
    }
    globalPrivilegeLevel := nextPrivilegeLevel
    io.redirect.valid := intoException || returnFromException
    io.flush := intoException || returnFromException
    io.redirect.bits := nextPC

    /* ================ Write logic ================ */
    val useUimm = io.in.bits.csrType === CSRRWI || io.in.bits.csrType === CSRRSI || io.in.bits.csrType === CSRRCI
    val rawWriteData = Mux(useUimm, io.in.bits.uimm, io.regRead.value)
    val csrWriteData = MuxLookup(io.in.bits.csrType, 0.U(32.W))(Seq(
        CSRRW -> rawWriteData,
        CSRRS -> (csrReadData | rawWriteData),
        CSRRC -> (csrReadData & (~rawWriteData)),
        CSRRWI -> rawWriteData,
        CSRRSI -> (csrReadData | rawWriteData),
        CSRRCI -> (csrReadData & (~rawWriteData))
    ))
    when(intoException){
        when(delegException){
            scause := Cat(interruptOccur, io.request.exceptionCode) // TODO: 其实这里还需要一个4-16译码器
            stval_reg := io.request.exceptionValue
            status.spp := globalPrivilegeLevel
            status.spie := status.sie
            sepc_reg := Cat(io.request.exceptionPC(31, 1), 0.U(1.W))
            status.sie := false.B
        }.otherwise{
            mcause := Cat(interruptOccur, io.request.exceptionCode) // TODO: 其实这里还需要一个4-16译码器
            mtval_reg := io.request.exceptionValue
            status.mpp := globalPrivilegeLevel
            status.mpie := status.mie
            mepc_reg := Cat(io.request.exceptionPC(31, 1), 0.U(1.W))
            status.mie := false.B
        }
    }.elsewhen(returnFromException){
        when(globalPrivilegeLevel === M_LEVEL){
            status.mie := status.mpie
            status.mpie := true.B
            status.mpp := U_LEVEL
        }.elsewhen(globalPrivilegeLevel === S_LEVEL){
            status.sie := status.spie
            status.spie := true.B
            status.spp := U_LEVEL
        }
    }.elsewhen(io.in.valid && io.in.bits.writeReg && canWrite){
        switch(io.in.bits.csrAddr){
            is(CSR_MSTATUS_ADDR){
                status := csrWriteData.asTypeOf(new csr_status_t)
            }
            is(CSR_MTVEC_ADDR){
                mtvec := csrWriteData.asTypeOf(new csr_tvec_t)
            }
            is(CSR_MIP_ADDR){
                ip := csrWriteData.asTypeOf(new csr_ip_t)
            }
            is(CSR_MIE_ADDR){
                ie := csrWriteData.asTypeOf(new csr_ie_t)
            }
            is(CSR_MSCRATCH_ADDR ){
                mscratch_reg := csrWriteData
            }
            is(CSR_MEPC_ADDR){
                mepc_reg := csrWriteData
            }
            is(CSR_MCAUSE_ADDR){
                mcause := csrWriteData.asTypeOf(new csr_cause_t)
            }
            is(CSR_MHARTID_ADDR){
                mhartid_reg := csrWriteData
            }
            is(CSR_MIDELEG_ADDR){
                mideleg_reg := csrWriteData
            }
            is(CSR_MEDELEG_ADDR){
                medeleg_reg := csrWriteData
            }
            is(CSR_MTVAL_ADDR){
                mtval_reg := csrWriteData
            }
            is(CSR_SSTATUS_ADDR){
                status := csrWriteData.asTypeOf(new csr_status_t)
            }
            is(CSR_STVEC_ADDR){
                stvec := csrWriteData.asTypeOf(new csr_tvec_t)
            }
            is(CSR_SIP_ADDR){
                ip := csrWriteData.asTypeOf(new csr_ip_t)
            }
            is(CSR_SIE_ADDR){
                ie := csrWriteData.asTypeOf(new csr_ie_t)
            }
            is(CSR_SSCRATCH_ADDR){
                sscratch_reg := csrWriteData
            }
            is(CSR_SEPC_ADDR){
                sepc_reg := csrWriteData
            }
            is(CSR_SCAUSE_ADDR){
                scause := csrWriteData.asTypeOf(new csr_cause_t)
            }
        }
    }
}