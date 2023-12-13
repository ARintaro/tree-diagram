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
  val interrupt = Bool()
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

    val interruptInitializing = Output(Bool())
  })

  val ctrlIO = IO(new Bundle {
    val redirect = Output(new RedirectRequest)
    val clearICache = Output(Bool())
    val clearTLB = Output(Bool())
  })

  val base :: excited :: Nil = Enum(2)
  val state = RegInit(base)

  // TODO: Clear TLB
  ctrlIO.clearTLB := false.B

  /* ================ global privilege level ================ */
  val globalPrivilegeLevel = RegInit(M_LEVEL)
  BoringUtils.addSource(globalPrivilegeLevel, "globalPrivilegeLevel")

  /* ================= init ================= */
  io.regRead.id := 0.U
  io.regWrite.id := 0.U
  io.regWrite.value := 0.U
  io.regWrite.valid := false.B
  ctrlIO.clearICache := false.B
  ctrlIO.redirect.target := 0x10000007L.U
  ctrlIO.redirect.valid := false.B

  when(io.exc.valid) {
    if (DebugConfig.printException) {
      DebugUtils.Print("[EXCU]!!!Exception")
      DebugUtils.Print(cf" valid: ${io.exc.valid}")
      DebugUtils.Print(cf" precisePC: 0x${Hexadecimal(io.exc.precisePC)}")
      DebugUtils.Print(cf" csrTag: ${io.exc.csrTag}")
      DebugUtils.Print(cf" exceptionCode: ${io.exc.exceptionCode}")
      DebugUtils.Print(
        cf" rawExceptionValue2: 0x${Hexadecimal(io.exc.rawExceptionValue2)}"
      )

      DebugUtils.Print("[EXCU]!!!Reference")
      DebugUtils.Print(cf" csrType: ${io.reference.csrType}")
      DebugUtils.Print(cf" csrAddr: ${io.reference.csrAddr}")
      DebugUtils.Print(cf" prs: ${io.reference.prs}")
      DebugUtils.Print(cf" prd: ${io.reference.prd}")
      DebugUtils.Print(cf" uimm: ${io.reference.uimm}")
      DebugUtils.Print(cf" writeCsrEn: ${io.reference.writeCsrEn}")
      DebugUtils.Print(cf" readCsrEn: ${io.reference.readCsrEn}")
      DebugUtils.Print(cf" regWrite id: ${io.regWrite}")

    }
  }

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

  val satp = RegInit(0.U.asTypeOf(new csr_satp_t)) // satp

  val mtime = WireInit(0.U(32.W))
  BoringUtils.addSink(mtime, "mtimeL")
  val mtimeh = WireInit(0.U(32.W))
  BoringUtils.addSink(mtimeh, "mtimeH")

  // BoringUtils.addSource(status.sum, "statusSum")
  BoringUtils.addSource(satp, "satp")

  val mstatusDebug = WireInit(0.U(32.W))
  mstatusDebug := status.reg(M_LEVEL)
  val mcauseDebug = WireInit(0.U(32.W))
  mcauseDebug := mcause.asUInt
  val mtvecDebug = WireInit(0.U(32.W))
  mtvecDebug := mtvec.asUInt
  val mepcDebug = WireInit(0.U(32.W))
  mepcDebug := mepc_reg
  val mieDebug = WireInit(0.U(32.W))
  mieDebug := ie.reg(M_LEVEL)
  val mipDebug = WireInit(0.U(32.W))
  mipDebug := ip.reg(M_LEVEL)
  val mscratchDebug = WireInit(0.U(32.W))
  mscratchDebug := mscratch_reg
  val mtvalDebug = WireInit(0.U(32.W))
  mtvalDebug := mtval_reg
  val medelegDebug = WireInit(0.U(32.W))
  medelegDebug := medeleg_reg
  val midelegDebug = WireInit(0.U(32.W))
  midelegDebug := mideleg_reg
  val mhartidDebug = WireInit(0.U(32.W))
  mhartidDebug := mhartid_reg

  val sstatusDebug = WireInit(0.U(32.W))
  sstatusDebug := status.reg(S_LEVEL)
  val scauseDebug = WireInit(0.U(32.W))
  scauseDebug := scause.asUInt
  val stvecDebug = WireInit(0.U(32.W))
  stvecDebug := stvec.asUInt
  val sepcDebug = WireInit(0.U(32.W))
  sepcDebug := sepc_reg
  val sieDebug = WireInit(0.U(32.W))
  sieDebug := ie.reg(S_LEVEL)
  val sipDebug = WireInit(0.U(32.W))
  sipDebug := ip.reg(S_LEVEL)
  val sscratchDebug = WireInit(0.U(32.W))
  sscratchDebug := sscratch_reg
  val stvalDebug = WireInit(0.U(32.W))
  stvalDebug := stval_reg
  val satpDebug = WireInit(0.U(32.W))
  satpDebug := satp.asUInt

  // TODO: satp

  val f_intoException = RegInit(false.B)
  val f_returnFromException = RegInit(false.B)
  val f_conductCsrInst = RegInit(false.B)
  val f_conductFencei = RegInit(false.B)
  val f_conductSfence = RegInit(false.B)
  // val f_csrType = RegInit(0.U(CSR_WIDTH))
  val f_generalizedExceptionCode = RegInit(0.U(InsConfig.EXCEPTION_WIDTH))
  val f_interruptPending = RegInit(false.B)
  val f_exceptionValue = RegInit(0.U(32.W))
  val f_csrWriteData = RegInit(0.U(32.W))
  val f_writeCsrEn = RegInit(false.B)
  val f_canWriteCsr = RegInit(false.B)
  val f_precisePC = RegInit(0.U(32.W))
  val f_csrAddr = RegInit(0.U(CSR_ADDR_WIDTH))
  val f_delegException = RegInit(false.B)
  val f_nextPC = RegInit(0.U(32.W))
  val f_nextPriv = RegInit(0.U(2.W))

  /* ================ Post Decode ==============*/
  val intoException = !io.exc.csrTag && io.exc.valid // NOTE: 时钟中断也是广义异常，处理方式类似
  val returnFromException =
    io.exc.csrTag && io.exc.valid && (io.reference.csrType === MRET || io.reference.csrType === SRET) && !intoException
  val conductCsrInst =
    io.exc.valid && io.exc.csrTag && !returnFromException && !intoException && (
      io.reference.csrType === CSRRW || io.reference.csrType === CSRRS || io.reference.csrType === CSRRC || io.reference.csrType === CSRRWI || io.reference.csrType === CSRRSI || io.reference.csrType === CSRRCI
    )
  val conductFencei =
    io.exc.valid && !intoException && !returnFromException && io.reference.csrType === FENCEI
  val conductSfence =
    io.exc.valid && !intoException && !returnFromException && io.reference.csrType === SFENCE_VMA

  when(io.exc.valid) {
    if (DebugConfig.printException) {
      DebugUtils.Print("[EXCU]!!!Post Decode")
      DebugUtils.Print(cf" intoException: ${intoException}")
      DebugUtils.Print(cf" returnFromException: ${returnFromException}")
      DebugUtils.Print(cf" conductCsrInst: ${conductCsrInst}")
    }
  }
  val uimm32 = Cat(Fill(27, 0.U), io.reference.uimm)

  /* ================ Interrupt logic ================ */
  val updateMtip = Wire(Bool())
  BoringUtils.addSink(updateMtip, "mtimeExceeded")
  ip.mtip := updateMtip
  val meiOccur = ie.meie & ip.meip
  val seiOccur = ie.seie & ip.seip
  val mtiOccur = ie.mtie & ip.mtip // 时钟中断
  val stiOccur = ie.stie & ip.stip
  val msiOccur = ie.msie & ip.msip
  val ssiOccur = ie.ssie & ip.ssip
  val mInterrupt = meiOccur | mtiOccur | msiOccur
  val sInterrupt = seiOccur | stiOccur | ssiOccur
  io.interruptInitializing := MuxCase(
    false.B,
    Seq(
      // (globalPrivilegeLevel === M_LEVEL) -> (mInterrupt & status.mie),
      (globalPrivilegeLevel === S_LEVEL) -> (mInterrupt | (sInterrupt & status.sie)),
      (globalPrivilegeLevel === U_LEVEL) -> (mInterrupt | sInterrupt)
    )
  )

  if (DebugConfig.printException) {
    DebugUtils.Print(cf"mInterrupt: ${mInterrupt}")
    DebugUtils.Print(cf"sInterrupt: ${sInterrupt}")
    DebugUtils.Print(cf"mie: ${status.mie}")
    DebugUtils.Print(cf"mtip: ${ip.mtip}")
    DebugUtils.Print(cf"mtie: ${ie.mtie}")
  }

  val interruptCode = Wire(UInt(InsConfig.EXCEPTION_WIDTH))
  interruptCode := MuxCase(
    0.U,
    Seq(
      meiOccur -> IT_M_EXT_INT,
      msiOccur -> IT_M_SOFT_INT,
      mtiOccur -> IT_M_TIMER_INT,
      seiOccur -> IT_S_EXT_INT,
      stiOccur -> IT_S_TIMER_INT,
      ssiOccur -> IT_S_SOFT_INT
    )
  )

  val interruptPending = io.exc.interrupt

  val generalizedExceptionCode =
    Mux(interruptPending, interruptCode, io.exc.exceptionCode)

  if (DebugConfig.printException) {
    DebugUtils.Print(cf"Now is in state ${globalPrivilegeLevel}")
    when(io.interruptInitializing) {
      DebugUtils.Print("Interrupt Initializing")
    }
    when(interruptPending) {
      DebugUtils.Print("Interrupt Pending")
    }
  }

  /* ================check privilege ================ */
  val csrReadOnly = io.reference.csrAddr(11, 10) === 3.U
  val csrPrivilegeLevel = io.reference.csrAddr(9, 8)
  val canReadCsr = csrPrivilegeLevel <= globalPrivilegeLevel
  val canWriteCsr = csrPrivilegeLevel <= globalPrivilegeLevel && !csrReadOnly

  /* ================ Read CSR logic ================ */
  val csrReadData = MuxLookup(io.reference.csrAddr, 0.U(32.W))(
    Seq(
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
      CSR_STVAL_ADDR -> stval_reg,
      CSR_SATP_ADDR -> satp.asUInt,
      CSR_TIME_ADDR -> mtime,
      CSR_TIMEH_ADDR -> mtimeh
    )
  )
  when(conductCsrInst && canReadCsr && io.reference.readCsrEn) {
    io.regWrite.id := io.reference.prd
    io.regWrite.value := csrReadData
    io.regWrite.valid := true.B
    if (DebugConfig.printException) {
      DebugUtils.Print("[EXCU]Read CSR; Write Register")
      DebugUtils.Print(cf" id: ${io.regWrite.id}")
      DebugUtils.Print(cf" value: 0x${Hexadecimal(io.regWrite.value)}")
      DebugUtils.Print(cf" valid: ${io.regWrite.valid}")
    }
  }

  /* ========== Trap deleg logic ========== */
  val delegException = Wire(Bool())
  delegException := MuxCase(
    medeleg_reg(Cat(0.U, io.exc.exceptionCode)),
    Seq(
      (globalPrivilegeLevel === M_LEVEL) -> false.B,
      interruptPending -> mideleg_reg(Cat(0.U, interruptCode))
    )
  )

  /* ============ Redirect logic ============ */
  val nextPC = WireInit(0.U(32.W))
  val nextPrivilegeLevel = WireInit(globalPrivilegeLevel)
  when(intoException || interruptPending) {
    nextPC := Mux(
      delegException,
      Mux(
        stvec.mode === 0.U,
        Cat(stvec.base, Fill(2, 0.U)),
        Cat(stvec.base, Fill(2, 0.U)) + (generalizedExceptionCode << 2.U)
      ),
      Mux(
        mtvec.mode === 0.U,
        Cat(mtvec.base, Fill(2, 0.U)),
        Cat(mtvec.base, Fill(2, 0.U)) + (generalizedExceptionCode << 2.U)
      )
    )
    nextPrivilegeLevel := Mux(delegException, S_LEVEL, M_LEVEL)
  }.elsewhen(returnFromException) {
    nextPC := Mux(globalPrivilegeLevel === M_LEVEL, mepc_reg, sepc_reg)
    when(io.reference.csrType === MRET) {
      // MRET
      nextPrivilegeLevel := Mux(
        globalPrivilegeLevel === M_LEVEL,
        status.mpp,
        status.spp
      )
    }.otherwise {
      // SRET
      nextPrivilegeLevel := Mux(
        globalPrivilegeLevel === M_LEVEL,
        status.mpp,
        status.spp
      )
    }
  }.otherwise {
    nextPC := io.exc.precisePC + 4.U
    nextPrivilegeLevel := globalPrivilegeLevel
  }

  when(io.exc.valid) {
    if (DebugConfig.printException) {
      DebugUtils.Print("[EXCU]Redirect")

    }
  }

  /* ================ Write CSR logic ================ */
  val useUimm =
    io.reference.csrType === CSRRWI || io.reference.csrType === CSRRSI || io.reference.csrType === CSRRCI
  io.regRead.id := io.reference.prs
  val rawWriteData = Mux(useUimm, uimm32, io.regRead.value)
  val csrWriteData = MuxLookup(io.reference.csrType, 0.U(32.W))(
    Seq(
      CSRRW -> rawWriteData,
      CSRRS -> (csrReadData | rawWriteData),
      CSRRC -> (csrReadData & (~rawWriteData)),
      CSRRWI -> rawWriteData,
      CSRRSI -> (csrReadData | rawWriteData),
      CSRRCI -> (csrReadData & (~rawWriteData))
    )
  )

  when(io.exc.valid) {
    if (DebugConfig.printException) {
      DebugUtils.Print("[EXCU]Write CSR; Read Register")
      DebugUtils.Print(cf" csrWriteData: 0x${Hexadecimal(csrWriteData)}")
    }
  }

  val exceptionValue = Mux(
    io.exc.exceptionCode === EC_ILLEGAL,
    io.rawExceptionValue1,
    io.exc.rawExceptionValue2
  ) // FIXME: 这里之后还要完善

  switch(state) {
    is(base) {
      when(io.exc.valid) {
        state := excited
        f_intoException := intoException
        f_returnFromException := returnFromException
        f_conductCsrInst := conductCsrInst
        f_conductFencei := conductFencei
        f_conductSfence := conductSfence
        // f_csrType := io.reference.csrType
        f_generalizedExceptionCode := generalizedExceptionCode
        f_interruptPending := interruptPending
        f_canWriteCsr := canWriteCsr
        f_exceptionValue := exceptionValue
        f_csrWriteData := csrWriteData
        f_writeCsrEn := io.reference.writeCsrEn
        f_precisePC := io.exc.precisePC
        f_csrAddr := io.reference.csrAddr
        f_delegException := delegException
        f_nextPC := nextPC
        f_nextPriv := nextPrivilegeLevel
      }
    }
    is(excited) {
      state := base
      globalPrivilegeLevel := f_nextPriv
      assert(!io.exc.valid)
      ctrlIO.redirect.valid := true.B
      ctrlIO.redirect.target := f_nextPC

      when(f_intoException || f_interruptPending) {
        when(f_delegException) {
          scause := Cat(
            f_interruptPending,
            Fill(27, 0.U),
            f_generalizedExceptionCode(3, 0)
          ).asTypeOf(new csr_cause_t)
          stval_reg := f_exceptionValue
          status.spp := globalPrivilegeLevel
          status.spie := status.sie
          sepc_reg := Cat(f_precisePC(31, 1), 0.U(1.W))
          status.sie := false.B
        }.otherwise {
          mcause := Cat(
            f_interruptPending,
            Fill(27, 0.U),
            f_generalizedExceptionCode(3, 0)
          ).asTypeOf(new csr_cause_t)
          mtval_reg := f_exceptionValue
          status.mpp := globalPrivilegeLevel
          status.mpie := status.mie
          mepc_reg := Cat(f_precisePC(31, 1), 0.U(1.W))
          status.mie := false.B
        }
      }.elsewhen(f_returnFromException) {
        when(globalPrivilegeLevel === M_LEVEL) {
          status.mie := status.mpie
          status.mpie := true.B
          status.mpp := U_LEVEL
        }.elsewhen(globalPrivilegeLevel === S_LEVEL) {
          status.sie := status.spie
          status.spie := true.B
          status.spp := U_LEVEL
        }
      }.elsewhen(f_conductCsrInst && f_writeCsrEn && f_canWriteCsr) {
        switch(f_csrAddr) {
          is(CSR_MSTATUS_ADDR) {
            status := f_csrWriteData.asTypeOf(new csr_status_t)
          }
          is(CSR_MTVEC_ADDR) {
            assert(f_csrWriteData(1, 0) === 0.U, "Misaligned mtvec")
            mtvec := f_csrWriteData.asTypeOf(new csr_tvec_t)
          }
          is(CSR_MIP_ADDR) {
            when(globalPrivilegeLevel === M_LEVEL) {
              ip.stip := f_csrWriteData(5)
            }
          }
          is(CSR_MIE_ADDR) {
            ie := f_csrWriteData.asTypeOf(new csr_ie_t)
          }
          is(CSR_MSCRATCH_ADDR) {
            mscratch_reg := f_csrWriteData
          }
          is(CSR_MEPC_ADDR) {
            mepc_reg := Cat(f_csrWriteData(31, 1), 0.U(1.W))
          }
          is(CSR_MCAUSE_ADDR) {
            mcause.interrupt := f_csrWriteData(31)
            assert(f_csrWriteData(30, 4) === 0.U)
            mcause.exceptionCode := f_csrWriteData(30, 0)
          }
          is(CSR_MHARTID_ADDR) {
            mhartid_reg := f_csrWriteData
          }
          is(CSR_MIDELEG_ADDR) {
            mideleg_reg := f_csrWriteData
          }
          is(CSR_MEDELEG_ADDR) {
            medeleg_reg := f_csrWriteData
          }
          is(CSR_MTVAL_ADDR) {
            mtval_reg := f_csrWriteData
            // mtval_reg(11) := false.B
          }
          is(CSR_SSTATUS_ADDR) {
            status.sie := f_csrWriteData(1)
            status.spie := f_csrWriteData(5)
            status.spp := f_csrWriteData(8)
            status.sum := f_csrWriteData(18)
          }
          is(CSR_STVEC_ADDR) {
            when(f_csrWriteData(1, 0) === 0.U) {
              stvec := f_csrWriteData.asTypeOf(new csr_tvec_t)
            }
          }
          is(CSR_SIP_ADDR) {
            // stip read-only
          }
          is(CSR_SIE_ADDR) {
            ie.seie := f_csrWriteData(9)
            ie.stie := f_csrWriteData(5)
            ie.ssie := f_csrWriteData(1)
          }
          is(CSR_SSCRATCH_ADDR) {
            sscratch_reg := f_csrWriteData
          }
          is(CSR_SEPC_ADDR) {
            sepc_reg := Cat(f_csrWriteData(31, 1), 0.U(1.W))
          }
          is(CSR_SCAUSE_ADDR) {
            scause.interrupt := f_csrWriteData(31)
            assert(f_csrWriteData(30, 4) === 0.U)
            scause.exceptionCode := f_csrWriteData(30, 0)
          }
          is(CSR_STVAL_ADDR) {
            stval_reg := f_csrWriteData
          }
          is(CSR_SATP_ADDR) {
            satp := f_csrWriteData.asTypeOf(new csr_satp_t)
          }
        }
      }.elsewhen(f_conductFencei) {
        ctrlIO.clearICache := true.B
      }.elsewhen(f_conductSfence) {
        ctrlIO.clearTLB := true.B
      }
    }
  }
}
