package core

import chisel3._
import chisel3.util._

import RV32IPattern._

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

  val signals = ListLookup(data,
                        /* 0       , 1       , 2       , 3       , 4      , 5        , 6      , 7       , 8       , 9       */ 
                        /* aluType , bruType , selOP1  , selOP2  , writeRd, immType  , valid  , iqtType , memWrite, memLen  */
    List(                  ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , false.B, IMMT_I   , false.B, IQT_INT , false.B , MEM_WORD),
    Array(      
      luiPattern   -> List(ALU_ADD , BRU_NONE, OP1_ZERO, OP2_IMM , true.B , IMMT_U   , true.B , IQT_INT , false.B , MEM_WORD),
      auipcPattern -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_IMM , true.B , IMMT_U   , true.B , IQT_INT , false.B , MEM_WORD),
      jalPattern   -> List(ALU_ADD , BRU_NONE, OP1_PC  , OP2_IMM , true.B , IMMT_J   , true.B , IQT_INT , false.B , MEM_WORD),
      jalrPattern  -> List(ALU_ADD , BRU_JALR, OP1_PC  , OP2_FOUR, true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      beqPattern   -> List(ALU_ADD , BRU_EQ  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      bnePattern   -> List(ALU_ADD , BRU_NE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      bltPattern   -> List(ALU_ADD , BRU_LT  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      bgePattern   -> List(ALU_ADD , BRU_GE  , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      bltuPattern  -> List(ALU_ADD , BRU_LTU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      bgeuPattern  -> List(ALU_ADD , BRU_GEU , OP1_PC  , OP2_IMM , false.B, IMMT_B   , true.B , IQT_INT , false.B , MEM_WORD),
      lbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_MEM , false.B , MEM_BYTE),
      lhPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_MEM , false.B , MEM_HALF),
      lwPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_MEM , false.B , MEM_WORD),
      lbuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_MEM , false.B , MEM_BYTE),
      lhuPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_MEM , false.B , MEM_HALF),
      sbPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , true.B , IQT_MEM , true.B  , MEM_BYTE),
      shPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , true.B , IQT_MEM , true.B  , MEM_HALF),
      swPattern    -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , false.B, IMMT_S   , true.B , IQT_MEM , true.B  , MEM_WORD),
      addiPattern  -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      sltiPattern  -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      sltiuPattern -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      xoriPattern  -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      oriPattern   -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      andiPattern  -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      slliPattern  -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      srliPattern  -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      sraiPattern  -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_IMM , true.B , IMMT_I   , true.B , IQT_INT , false.B , MEM_WORD),
      addPattern   -> List(ALU_ADD , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      subPattern   -> List(ALU_SUB , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      sllPattern   -> List(ALU_SLL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      sltPattern   -> List(ALU_SLT , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      sltuPattern  -> List(ALU_SLTU, BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      xorPattern   -> List(ALU_XOR , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      srlPattern   -> List(ALU_SRL , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      sraPattern   -> List(ALU_SRA , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      orPattern    -> List(ALU_OR  , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
      andPattern   -> List(ALU_AND , BRU_NONE, OP1_RS1 , OP2_RS2 , true.B , IMMT_NONE, true.B , IQT_INT , false.B , MEM_WORD),
  ))

  io.out.aluType := signals(0)
  io.out.bruType := signals(1)
  io.out.selOP1 := signals(2)
  io.out.selOP2 := signals(3)
  io.out.writeRd := signals(4)
  io.out.immType := signals(5)
  io.out.valid := signals(6)
  io.out.iqtType := signals(7)
  io.out.memWrite := signals(8)
  io.out.memLen := signals(9)

  io.out.flush := false.B
  io.out.unique := false.B
  io.out.isJalr := data === jalrPattern

}

class DecodeUnit extends Module {
  val io = IO(new Bundle {
    // 连接到取指队列
    val in = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    
    // Decode Unit 必须所有指令一起出去
    val out = Vec(FrontendConfig.decoderNum, Output(new DecodedInstruction))
    val outReady = Input(Bool())
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val decoders = VecInit(Seq.fill(FrontendConfig.decoderNum)(Module(new Decoder).io))

  val result = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(0.U.asTypeOf(new DecodedInstruction))))

  val empty = !result.map(_.valid).reduce(_ || _)
  val ready = io.outReady || empty
  
  for (i <- 0 until FrontendConfig.decoderNum) {
    // 如果自己是气泡，不管后面准没准备好都可以ready
    io.in(i).ready := ready
    decoders(i).in := io.in(i).bits
    decoders(i).valid := io.in(i).valid

    io.out(i) := result(i)
  }

  when (ctrlIO.flush) {
    result.foreach(_.valid := false.B)
    io.out.foreach(_.valid := false.B)

  } .elsewhen(ready) {
    for (i <- 0 until FrontendConfig.decoderNum) {
      result(i) := decoders(i).out
    }

  }

}