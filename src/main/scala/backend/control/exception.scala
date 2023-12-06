package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import CsrConstants._
import PrivilegeLevel._
import InsConfig.ExceptionCode._

class NewException extends Bundle {
    val valid = Bool()
    val precisePC = UInt(32.W)
    val csrTag = Bool()
    val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)
    val rawExceptionValue2 = UInt(32.W)
}

class CsrInstruction
    extends Bundle
    with InstructionConstants
    with IssueInstruction {

    val csrType = UInt(CSR_WIDTH)
    val csrAddr = UInt(CSR_ADDR_WIDTH)
    val prs = UInt(BackendConfig.pregIdxWidth)
    val prd = UInt(BackendConfig.pregIdxWidth)
    val uimm = UInt(5.W)
    val writeCsrEn = Bool()
    val readCsrEn = Bool()

    override def checkReady(busy: UInt): Bool = {
        return !busy(prs)
    }
}


class ExceptionUnit extends Module with InstructionConstants {
    val io = IO(new Bundle {
        val exc = Input(new NewException)
        val reference = Input(new CsrInstruction)
        val rawExceptionValue1 = Input(UInt(32.W))

        val regRead = new RegisterReadRequest
        val regWrite = new RegisterWriteRequest

        val redirect = Valid(UInt(BusConfig.ADDR_WIDTH))
    })

    val ctrlIO = IO(new Bundle {
        val flushPipeline = Output(Bool())
    })

    /* ================= init ================= */
    io.regRead.id := 0.U
    io.regWrite.id := 0.U
    io.regWrite.value := 0.U
    io.regWrite.valid := false.B



    /* ================ global privilege level ================ */
    val globalPrivilegeLevel = RegInit(M_LEVEL)
    BoringUtils.addSource(globalPrivilegeLevel, "globalPrivilegeLevel")

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

    /* ================ Post Decode ==============*/
    val intoException = !io.exc.csrTag && io.exc.valid
    val returnFromException = io.exc.csrTag && io.exc.valid && (io.reference.csrType === MRET || io.reference.csrType === SRET) && !intoException
    val conductCsrInst = io.exc.valid && io.exc.csrTag && !returnFromException && !intoException
    ctrlIO.flushPipeline := RegNext(io.exc.valid)
    val uimm32 = Cat(Fill(27, 0.U), io.reference.uimm)

    /* ================ Interrupt logic ================ */
    val updateMtip = Wire(Bool())
    BoringUtils.addSink(updateMtip, "timerInterrupt")
    ip.mtip := updateMtip
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
    val generalizedExceptionCode = Mux(interruptOccur, interruptCode, io.exc.exceptionCode)

    /* ================check privilege ================ */
    val csrReadOnly = io.reference.csrAddr(11, 10) === 3.U
    val csrPrivilegeLevel = io.reference.csrAddr(9, 8)
    val canReadCsr = csrPrivilegeLevel <= globalPrivilegeLevel
    val canWriteCsr = csrPrivilegeLevel <= globalPrivilegeLevel && !csrReadOnly

    /* ================ Read logic ================ */
    val csrReadData = MuxLookup(io.reference.csrAddr, 0.U(32.W))(Seq(
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
    when(conductCsrInst && canReadCsr && io.reference.readCsrEn){ 
        io.regWrite.id := io.reference.prd
        io.regWrite.value := csrReadData
        io.regWrite.valid := true.B
    }

    /* ==========Trap deleg logic ========== */
    val delegException = Wire(Bool())
    delegException := MuxCase(medeleg_reg(Cat(0.U, io.exc.exceptionCode)), Seq(
        (globalPrivilegeLevel === M_LEVEL) -> false.B,
        interruptOccur -> mideleg_reg(Cat(0.U, interruptCode)),
    ))

    /* ============ Redirect logic ============ */
    val nextPC = WireInit(0.U(32.W))
    val nextPrivilegeLevel = WireInit(globalPrivilegeLevel)
    when(intoException){
        nextPC := Mux(delegException,
            Mux(stvec.mode === 0.U,
            Cat(stvec.base, Fill(2, 0.U)),
            Cat(stvec.base, Fill(2, 0.U)) + (generalizedExceptionCode << 2.U)),
            Mux(mtvec.mode === 0.U,
            Cat(mtvec.base, Fill(2, 0.U)),
            Cat(mtvec.base, Fill(2, 0.U)) + (generalizedExceptionCode << 2.U))
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
    }.elsewhen(conductCsrInst){
        nextPC := io.exc.precisePC + 4.U
        nextPrivilegeLevel := globalPrivilegeLevel
    }
    globalPrivilegeLevel := nextPrivilegeLevel
    io.redirect.valid := intoException || returnFromException
    io.redirect.bits := nextPC

    /* ================ Write logic ================ */
    val useUimm = io.reference.csrType === CSRRWI || io.reference.csrType === CSRRSI || io.reference.csrType === CSRRCI
    io.regRead.id := io.reference.prs
    val rawWriteData = Mux(useUimm, uimm32, io.regRead.value)
    val csrWriteData = MuxLookup(io.reference.csrType, 0.U(32.W))(Seq(
        CSRRW -> rawWriteData,
        CSRRS -> (csrReadData | rawWriteData),
        CSRRC -> (csrReadData & (~rawWriteData)),
        CSRRWI -> rawWriteData,
        CSRRSI -> (csrReadData | rawWriteData),
        CSRRCI -> (csrReadData & (~rawWriteData))
    ))
    val exceptionValue = Mux(io.exc.exceptionCode === EC_ILLEGAL, io.rawExceptionValue1, io.exc.rawExceptionValue2)
    when(intoException){
        when(delegException){
            scause := Cat(interruptOccur, Fill(27, 0.U), generalizedExceptionCode(3, 0)).asTypeOf(new csr_cause_t)
            stval_reg := exceptionValue
            status.spp := globalPrivilegeLevel
            status.spie := status.sie
            sepc_reg := Cat(io.exc.precisePC(31, 1), 0.U(1.W))
            status.sie := false.B
        }.otherwise{
            mcause := Cat(interruptOccur, Fill(27, 0.U), generalizedExceptionCode(3, 0)).asTypeOf(new csr_cause_t)
            mtval_reg := exceptionValue
            status.mpp := globalPrivilegeLevel
            status.mpie := status.mie
            mepc_reg := Cat(io.exc.precisePC(31, 1), 0.U(1.W))
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
    }.elsewhen(io.exc.valid && io.reference.writeCsrEn && canWriteCsr){
        switch(io.reference.csrAddr){
            is(CSR_MSTATUS_ADDR){
                status := csrWriteData.asTypeOf(new csr_status_t)
            }
            is(CSR_MTVEC_ADDR){
                assert(csrWriteData(1, 0) === 0.U, "Misaligned mtvec")
                mtvec := csrWriteData.asTypeOf(new csr_tvec_t)
            }
            is(CSR_MIP_ADDR){
                when(globalPrivilegeLevel === M_LEVEL){
                    ip.stip := csrWriteData(5)
                }
            }
            is(CSR_MIE_ADDR){
                ie := csrWriteData.asTypeOf(new csr_ie_t)
            }
            is(CSR_MSCRATCH_ADDR ){
                mscratch_reg := csrWriteData
            }
            is(CSR_MEPC_ADDR){
                mepc_reg := Cat(csrWriteData(31, 1), 0.U(1.W))
            }
            is(CSR_MCAUSE_ADDR){
                mcause.interrupt := csrWriteData(31)
                assert(csrWriteData(30, 4) === 0.U)
                mcause.exceptionCode := csrWriteData(30, 0)
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
                // mtval_reg(11) := false.B
            }
            is(CSR_SSTATUS_ADDR){
                status.sie := csrWriteData(1)
                status.spie := csrWriteData(5)
                status.spp := csrWriteData(8)
            }
            is(CSR_STVEC_ADDR){
                when(csrWriteData(1, 0) === 0.U){
                    stvec := csrWriteData.asTypeOf(new csr_tvec_t)
                }
            }
            is(CSR_SIP_ADDR){
                // stip read-only
            }
            is(CSR_SIE_ADDR){
                ie.seie := csrWriteData(9)
                ie.stie := csrWriteData(5)
                ie.ssie := csrWriteData(1)
            }
            is(CSR_SSCRATCH_ADDR){
                sscratch_reg := csrWriteData
            }
            is(CSR_SEPC_ADDR){
                sepc_reg := Cat(csrWriteData(31, 1), 0.U(1.W))
            }
            is(CSR_SCAUSE_ADDR){
                scause.interrupt := csrWriteData(31)
                assert(csrWriteData(30, 4) === 0.U)
                scause.exceptionCode := csrWriteData(30, 0)
            }
            is(CSR_STVAL_ADDR){
                stval_reg := csrWriteData
            }
        }
    }
}