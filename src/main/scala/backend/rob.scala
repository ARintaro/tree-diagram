package core

import chisel3._
import chisel3.util._
import svsim.Backend
import javax.swing.DebugGraphics
import chisel3.util.experimental.BoringUtils

class RobEntry extends Bundle with InstructionConstants {
  val completed = Bool()
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val writeRd = Bool()
  // 写入寄存器逻辑ID
  val rdLidx = UInt(5.W)
  // 写入寄存器物理ID
  val rdPidx = UInt(BackendConfig.pregIdxWidth)

  val realJump = Bool()
  val predictJump = Bool()
  val jumpTargetError = Bool()
  val jumpTarget = UInt(BusConfig.ADDR_WIDTH)

  val exception = Bool()
  val exceptionCode = UInt(InsConfig.EXCEPTION_WIDTH)

  val storeBufferIdx = UInt(BackendConfig.storeBufferIdxWidth)
  val storeType = UInt(STORE_TYPE_WIDTH)

  val csrTag = Bool()

}

class NewRobRequest extends Bundle with InstructionConstants {
  val valid = Input(Bool())

  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  val writeRd = Input(Bool())
  val rdLidx = Input(UInt(5.W))
  val rdPidx = Input(UInt(BackendConfig.pregIdxWidth))
  val predictJump = Input(Bool())
  val predictJumpTarget = Input(UInt(BusConfig.ADDR_WIDTH))
  val exception = Input(Bool())
  val exceptionCode = Input(UInt(InsConfig.EXCEPTION_WIDTH))

  val idx = Output(UInt(BackendConfig.robIdxWidth))
}

class RobNewIO extends Bundle {
  val news = Vec(FrontendConfig.decoderNum, new NewRobRequest)
  val newsCount = Input(UInt((log2Ceil(FrontendConfig.decoderNum)).W))

  val restSize = Output(UInt((log2Ceil(BackendConfig.robSize)).W))
}

class RobReadPcRequest extends Bundle with InstructionConstants {
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
}

class RobCompleteRequest extends Bundle with InstructionConstants{

  val valid = Output(Bool())
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val jump = Output(Bool())
  val jumpTarget = Output(UInt(BusConfig.ADDR_WIDTH))
  val exception = Output(Bool())
  val exceptionCode = Output(UInt(InsConfig.EXCEPTION_WIDTH))
  val storeBufferIdx = Output(UInt(BackendConfig.storeBufferIdxWidth))
  val storeType = Output(UInt(STORE_TYPE_WIDTH))
  val csrTag = Output(Bool())

}

class ReorderBuffer extends Module with InstructionConstants {
  val newIO = IO(new RobNewIO)
  val completeIO = IO(
    Vec(BackendConfig.pipelineNum, Flipped(new RobCompleteRequest))
  )
  val commitsIO = IO(
    Vec(BackendConfig.maxCommitsNum, new CommitPregRequest)
  )
  val readPcIO = IO(
    Vec(BackendConfig.intPipelineNum, Flipped(new RobReadPcRequest))
  )
  val io = IO(new Bundle {
    val redirect = Valid(UInt(BusConfig.ADDR_WIDTH))
    val commitsStoreBuffer = Vec(BackendConfig.maxCommitsNum, new CommitStoreRequest)

    val head = Output(UInt(BackendConfig.robIdxWidth))
    val newException = Output(new NewException)
    val empty = Output(Bool())
    val uncertern = Output(Bool())
  })
  
  val ctrlIO = IO(new Bundle {
    val flushPipeline = Output(Bool())
    // val conductCsr = Output(Bool())
  })

  io.newException.valid := false.B
  io.newException.exceptionCode := 0.U
  io.newException.precisePC := 0.U
  io.newException.rawExceptionValue2 := 0.U
  io.newException.csrTag := false.B

  dontTouch(io.uncertern)

  val entries = RegInit(
    VecInit(Seq.fill(BackendConfig.robSize)(0.U.asTypeOf(new RobEntry)))
  )

  val head = RegInit(0.U(BackendConfig.robIdxWidth))
  val tail = RegInit(0.U(BackendConfig.robIdxWidth))

  io.head := head
  io.empty := head === tail

  val count = tail - head

  newIO.restSize := (BackendConfig.robSize - 1).U - count


  // 默认不发起重定向和冲刷请求

  io.redirect.valid := false.B
  

  ctrlIO.flushPipeline := false.B

  // 处理rename阶段发送的ROB新建请求

  for (i <- 0 until FrontendConfig.decoderNum) {
    val idx = tail + i.U
    when(newIO.news(i).valid) {
      val entry = Wire(new RobEntry)
      entry.completed := false.B

      entry.vaddr := newIO.news(i).vaddr
      entry.writeRd := newIO.news(i).writeRd
      entry.rdLidx := newIO.news(i).rdLidx
      entry.rdPidx := newIO.news(i).rdPidx
      entry.predictJump := newIO.news(i).predictJump
      entry.realJump := DontCare
      entry.jumpTargetError := DontCare
      entry.jumpTarget := newIO.news(i).predictJumpTarget
      entry.exception := newIO.news(i).exception
      entry.exceptionCode := newIO.news(i).exceptionCode
      entry.storeBufferIdx := DontCare
      entry.storeType := DontCare
      entry.csrTag := false.B
    
      entries(idx) := entry
    }
    newIO.news(i).idx := idx
  }

  tail := tail + newIO.newsCount

  // 处理流水线中查询pc的请求
  for (i <- 0 until BackendConfig.intPipelineNum) {
    val pcRead = readPcIO(i)
    pcRead.vaddr := entries(pcRead.robIdx).vaddr
  }

  // 处理流水线写回的complete请求
  for (i <- 0 until BackendConfig.pipelineNum) {
    val complete = completeIO(i)
    val entry = entries(complete.robIdx)
    when(complete.valid) {
      entry.completed := true.B
      entry.realJump := complete.jump
      // JALR
      entry.jumpTargetError := complete.jump && entry.jumpTarget =/= complete.jumpTarget
      entry.jumpTarget := complete.jumpTarget
      entry.exception := entry.exception | complete.exception
      entry.storeBufferIdx := complete.storeBufferIdx
      entry.storeType := complete.storeType
      entry.csrTag := complete.csrTag
      when(complete.exception) {
        entry.exceptionCode := complete.exceptionCode
      }
    }
  }

  // 处理Commit
  val inQueueMask = MaskUtil.GetValidMask(head, tail)

  val commitValidsOne = Wire(Vec(BackendConfig.maxCommitsNum, Bool()))
  val commitEntry = Wire(Vec(BackendConfig.maxCommitsNum, new RobEntry))

  for (i <- 0 until BackendConfig.maxCommitsNum) {
    val idx = head + i.U
    commitValidsOne(i) := entries(idx).completed && inQueueMask(idx)
    commitEntry(i) := entries(idx)
  }

  val commitJumpValid =
    VecInit(commitEntry.map(x => !x.jumpTargetError && x.realJump === x.predictJump))

  val afterCsrInvalidMask = VecInit(commitEntry.map(_.csrTag).scan(false.B)(_ || _).init)

  val commitValidsTwo = commitValidsOne
    .zip(commitJumpValid)
    .zip(commitEntry)
    .zip(afterCsrInvalidMask)
    .map { case (((x, y), z), w) =>
      x && y && !z.exception && !w
    }
    .scan(true.B)(_ && _)
    .tail

  val allValidTwo = commitValidsTwo.reduce(_ && _)
  val firstInvalidIdx = PriorityEncoder(commitValidsTwo.map(!_))

  val invalidEntry = commitEntry(firstInvalidIdx)

  val commitValidsFinal = WireInit(VecInit(commitValidsTwo))

  
  io.redirect.bits := 0x10000001L.U
  when(!allValidTwo && commitValidsOne(firstInvalidIdx)) {
    io.redirect.valid := true.B
    ctrlIO.flushPipeline := true.B

    when(invalidEntry.exception) {
      io.newException.valid := true.B
      io.newException.exceptionCode := invalidEntry.exceptionCode
      io.newException.precisePC := invalidEntry.vaddr
      io.newException.csrTag := invalidEntry.csrTag
      io.newException.rawExceptionValue2 := 0.U // TODO: 关于mtval的赋值，我们还不考虑
      if (DebugConfig.printException) {
        DebugUtils.Print("=== Exception ===")
        DebugUtils.Print(cf"Commit Exception ${invalidEntry.exceptionCode}")
        DebugUtils.Print("=== END ===")
      }
      commitValidsFinal(firstInvalidIdx) := true.B
      // assert(invalidEntry.exceptionCode === InsConfig.ExceptionCode.EC_BREAKPOINT)
    }.elsewhen(commitJumpValid(firstInvalidIdx)) {
      // 分支预测失败
      commitValidsFinal(firstInvalidIdx) := true.B
      when(invalidEntry.jumpTargetError) {
        // 跳转地址错误
        DebugUtils.Print(cf"RobIdx JumpTarget Error, New Target 0x${invalidEntry.jumpTarget}%x")
        io.redirect.bits := invalidEntry.jumpTarget
      }.otherwise {
        // 是否跳转错误
        DebugUtils.Print(cf"RobIdx Jump Error, New Jump ${invalidEntry.realJump} 0x${io.redirect.bits}%x")
        assert(invalidEntry.realJump =/= invalidEntry.predictJump)
        io.redirect.bits := Mux(
          invalidEntry.realJump,
          invalidEntry.jumpTarget,
          // invalidEntry.vaddr + 4.U
          0x10000002L.U
        )
      }
    }.elsewhen(afterCsrInvalidMask(firstInvalidIdx)) {
        // 提交执行csr指令
        DebugUtils.Print(cf"RobIdx CSR Instruction: PC = 0x${invalidEntry.vaddr}%x")
        io.newException.valid := true.B
        io.newException.exceptionCode := 0.U
        io.newException.precisePC := invalidEntry.vaddr
        io.newException.rawExceptionValue2 := 0.U
        io.newException.csrTag := invalidEntry.csrTag
    }
  }

  // 这里没有用寄存器暂存输出，时序扛不住的话需要加上
  commitsIO.zip(commitValidsFinal).zip(commitEntry).foreach {
    case ((out, valid), entry) => {
      out.valid := valid && entry.writeRd
      out.pregIdx := entry.rdPidx
      out.lregIdx := entry.rdLidx
    }
  }

  io.commitsStoreBuffer.zip(commitValidsFinal).zip(commitEntry).foreach {
    case ((out, valid), entry) => {
      out.idx := entry.storeBufferIdx
      out.valid := (valid && (entry.storeType === STORE_RAM || entry.storeType === STORE_MMIO))
    }
  }

  io.uncertern := commitValidsFinal.zip(commitEntry).map{
    case (valid, entry) => {
      valid && (entry.storeType === STORE_MMIO || entry.storeType === LOAD_MMIO) 
    }
  }.reduce(_ || _)

  head := head + PopCount(commitValidsFinal)

  val flush = ctrlIO.flushPipeline
  when(flush) {
    head := 0.U
    tail := 0.U
  }

  if (DebugConfig.printRob) {
    when(head =/= tail) {
      DebugUtils.Print("=== ROB ===")
      DebugUtils.Print(cf"IDX | OVER | Jv | vaddr | store_type")
      for (i <- 0 until BackendConfig.robSize) {
        val idx = head + i.U
        when(inQueueMask(idx)) {
          val entry = entries(idx)
          DebugUtils.Print(cf"${idx}  ${entry.completed} ${commitJumpValid(idx)}  0x${entry.vaddr}%x ${entry.storeType}")
        }
      }
      // print commitValidsFinal
      DebugUtils.Print(cf"commitValidsFinal: ${commitValidsFinal.asTypeOf(Vec(BackendConfig.maxCommitsNum, Bool()))}")
      DebugUtils.Print("=== END ===")
    }
  }

  assert(newIO.newsCount <= newIO.restSize)
  assert(PopCount(newIO.news.map(_.valid)) === newIO.newsCount)
}
