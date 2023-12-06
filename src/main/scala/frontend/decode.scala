package core

import chisel3._
import chisel3.util._


import RV32IPattern._
import InsConfig._
import InsConfig.ExceptionCode._
import chisel3.util.experimental.BoringUtils

class Decoder extends Module with InstructionConstants {
  val io = IO(new Bundle {
    val in = Input(new RawInstruction)
    // 这条指令内的数据是否有效
    // 当valid为true，且指令内数据解码错误时，才能触发错误指令异常
    val valid = Input(Bool())

    val out = Output(new DecodedInstruction)
  })
  

  io.out.vaddr := io.in.vaddr

  val data = io.in.inst
  io.out.inst := data

  io.out.valid := io.valid

  val signals = ListLookup(data,
                        /* 0       , 1       , 2       , 3       , 4      , 5        , 6        , 7       , 8       , 9       , 10           , 11     , 12     , 13     */
                        /* aluType , bruType , selOP1  , selOP2  , writeRd, immType  , exception, iqtType , memType , memLen  , exceptionCode, extType, csrTag , csrType*/
    List(                  ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , false.B, IMMT_I   , true.B   , IQT_INT , false.B , MEM_WORD, EC_ILLEGAL   , true.B , false.B, CSRNONE),
    Array(         true.B  ,
      luiPattern   -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_IMM , true.B , IMMT_U   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      auipcPattern -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_IMM , true.B , IMMT_U   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      jalPattern   -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_FOUR, true.B , IMMT_J   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      jalrPattern  -> List(ALU_ADD , BRU_JALR, OP1_PC  , OP2_FOUR, true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      beqPattern   -> List(ALU_ADD , BRU_EQ  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      bnePattern   -> List(ALU_ADD , BRU_NE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      bltPattern   -> List(ALU_ADD , BRU_LT  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      bgePattern   -> List(ALU_ADD , BRU_GE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      bltuPattern  -> List(ALU_ADD , BRU_LTU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      bgeuPattern  -> List(ALU_ADD , BRU_GEU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      lbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_BYTE, 0.U          , true.B , false.B, CSRNONE),
      lhPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_HALF, 0.U          , true.B , false.B, CSRNONE),
      lwPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      lbuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_BYTE, 0.U          , false.B, false.B, CSRNONE),
      lhuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_HALF, 0.U          , false.B, false.B, CSRNONE),
      sbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_BYTE, 0.U          , true.B , false.B, CSRNONE),
      shPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_HALF, 0.U          , true.B , false.B, CSRNONE),
      swPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      addiPattern  -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sltiPattern  -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sltiuPattern -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      xoriPattern  -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      oriPattern   -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      andiPattern  -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      slliPattern  -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      srliPattern  -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sraiPattern  -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      addPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      subPattern   -> List(ALU_SUB , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sllPattern   -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sltPattern   -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sltuPattern  -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      xorPattern   -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      srlPattern   -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      sraPattern   -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      orPattern    -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      andPattern   -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , false.B, CSRNONE),
      ecallPattern -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, true.B   , IQT_INT , false.B , MEM_WORD, EC_U_ENV_CALL, true.B , false.B, CSRNONE),
      ebreakPattern-> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, true.B   , IQT_INT , false.B , MEM_WORD, EC_BREAKPOINT, true.B , false.B, CSRNONE),
      csrrwPattern -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRW  ),
      csrrsPattern -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRS  ),
      csrrcPattern -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRC  ),
      csrrwiPattern-> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRWI ),
      csrrsiPattern-> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRSI ),
      csrrciPattern-> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , CSRRCI ),
      mretPattern  -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , MRET   ),
      sretPattern  -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B , true.B , SRET   ),
    ))
  

  io.out.aluType := signals(0)
  io.out.bruType := signals(1)
  io.out.selOP1 := signals(2)
  io.out.selOP2 := signals(3)
  io.out.writeRd := signals(4) & (io.in.inst(11, 7) =/= 0.U)
  io.out.immType := signals(5)
  io.out.exception := signals(6) | io.in.exception
  io.out.iqtType := signals(7)
  io.out.memType := signals(8)
  io.out.memLen := signals(9)
  io.out.exceptionCode := Mux(io.in.exception, io.in.exceptionCode, signals(10))
  io.out.extType := signals(11)

  io.out.csrTag := signals(11)
  io.out.csrType := signals(12)

  val csrSignals = ListLookup(data, List(false.B, false.B),
    Array(
      csrrwPattern -> List(true.B, io.in.inst(11, 7) =/= 0.U),
      csrrsPattern -> List(io.in.inst(19, 15) =/= 0.U, true.B),
      csrrcPattern -> List(io.in.inst(19, 15) =/= 0.U, true.B),
      csrrwiPattern-> List(true.B, io.in.inst(11, 7) =/= 0.U),
      csrrsiPattern-> List(io.in.inst(19, 15) =/= 0.U, true.B),
      csrrciPattern-> List(io.in.inst(19, 15) =/= 0.U, true.B)
    )
  )
  io.out.writeCsrEn := csrSignals(0)
  io.out.readCsrEn := csrSignals(1)
  
  // TODO : Predict Jump
  io.out.predictJump := false.B
  io.out.predictTarget := 0.U

  io.out.flush := false.B
  io.out.unique := false.B

}

class DecodeUnit extends Module {
  val io = IO(new Bundle {
    // 连接到取指队列
    val in = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    
    // Decode Unit 必须所有指令一起出去
    val out = Vec(FrontendConfig.decoderNum, Output(new DecodedInstruction))

    // outBuffer中的数据已经利用完成，可以写入新的数据
    val nextDone = Input(Bool())

    // 错误的指令
    val wrongInstruction = Output(UInt(InsConfig.INS_WIDTH))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val decoders = VecInit(Seq.fill(FrontendConfig.decoderNum)(Module(new Decoder).io))

  val outBuffer = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(0.U.asTypeOf(new DecodedInstruction))))

  val wrongInstructionBuffer = RegInit(0.U(InsConfig.INS_WIDTH))
  io.wrongInstruction := wrongInstructionBuffer
  
  for (i <- 0 until FrontendConfig.decoderNum) {
    // 如果自己是气泡，不管后面准没准备好都可以ready
    io.in(i).ready := io.nextDone
    decoders(i).in := io.in(i).bits
    decoders(i).valid := io.in(i).valid

    io.out(i) := outBuffer(i)
    
    wrongInstructionBuffer := Mux(io.in(i).valid 
                               && io.in(i).bits.exception
                               && io.in(i).bits.exceptionCode === EC_ILLEGAL
                               && wrongInstructionBuffer === 0.U,
                               io.in(i).bits.inst, 
                               wrongInstructionBuffer)
  }

  when (ctrlIO.flush) {
    outBuffer.foreach(_.valid := false.B)
    
  } .elsewhen(io.nextDone) {
    for (i <- 0 until FrontendConfig.decoderNum) {
      outBuffer(i) := decoders(i).out
    }

  }

}