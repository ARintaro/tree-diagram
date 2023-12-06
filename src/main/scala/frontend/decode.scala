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
                        /* 0       , 1       , 2       , 3       , 4      , 5        , 6        , 7       , 8       , 9       , 10           , 11      */
                        /* aluType , bruType , selOP1  , selOP2  , writeRd, immType  , exception, iqtType , memType , memLen  , exceptionCode, extType */
    List(                  ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , false.B, IMMT_I   , true.B   , IQT_INT , false.B , MEM_WORD, EC_ILLEGAL   , true.B),
    Array(        
      luiPattern   -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_IMM , true.B , IMMT_U   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      auipcPattern -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_IMM , true.B , IMMT_U   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      jalPattern   -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_FOUR, true.B , IMMT_J   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      jalrPattern  -> List(ALU_ADD , BRU_JALR, OP1_PC  , OP2_FOUR, true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      beqPattern   -> List(ALU_ADD , BRU_EQ  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      bnePattern   -> List(ALU_ADD , BRU_NE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      bltPattern   -> List(ALU_ADD , BRU_LT  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      bgePattern   -> List(ALU_ADD , BRU_GE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      bltuPattern  -> List(ALU_ADD , BRU_LTU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      bgeuPattern  -> List(ALU_ADD , BRU_GEU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      lbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_BYTE, 0.U          , true.B),
      lhPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_HALF, 0.U          , true.B),
      lwPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_WORD, 0.U          , true.B),
      lbuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_BYTE, 0.U          , false.B),
      lhuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_MEM , false.B , MEM_HALF, 0.U          , false.B),
      sbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_BYTE, 0.U          , true.B),
      shPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_HALF, 0.U          , true.B),
      swPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , false.B  , IQT_MEM , true.B  , MEM_WORD, 0.U          , true.B),
      addiPattern  -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sltiPattern  -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sltiuPattern -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      xoriPattern  -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      oriPattern   -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      andiPattern  -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      slliPattern  -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      srliPattern  -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sraiPattern  -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      addPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      subPattern   -> List(ALU_SUB , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sllPattern   -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sltPattern   -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sltuPattern  -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      xorPattern   -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      srlPattern   -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      sraPattern   -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      orPattern    -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      andPattern   -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, false.B  , IQT_INT , false.B , MEM_WORD, 0.U          , true.B),
      ecallPattern -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, true.B   , IQT_INT , false.B , MEM_WORD, EC_U_ENV_CALL, true.B),
      ebreakPattern-> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_ZERO, false.B, IMMT_NONE, true.B   , IQT_INT , false.B , MEM_WORD, EC_BREAKPOINT, true.B),
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

  
  // TODO : Predict Jump
  io.out.predictJump := io.in.jump
  io.out.predictTarget := io.in.jumpTarget

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
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val decoders = VecInit(Seq.fill(FrontendConfig.decoderNum)(Module(new Decoder).io))

  val outBuffer = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(0.U.asTypeOf(new DecodedInstruction))))

  for (i <- 0 until FrontendConfig.decoderNum) {
    // 如果自己是气泡，不管后面准没准备好都可以ready
    io.in(i).ready := io.nextDone
    decoders(i).in := io.in(i).bits
    decoders(i).valid := io.in(i).valid

    io.out(i) := outBuffer(i)
  }

  when (ctrlIO.flush) {
    outBuffer.foreach(_.valid := false.B)
    
  } .elsewhen(io.nextDone) {
    for (i <- 0 until FrontendConfig.decoderNum) {
      outBuffer(i) := decoders(i).out
    }

  }

}