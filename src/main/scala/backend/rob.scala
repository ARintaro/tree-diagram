package core

import chisel3._
import chisel3.util._
import svsim.Backend
import javax.swing.DebugGraphics
import chisel3.util.experimental.BoringUtils
import InsConfig._

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

  // val storeBufferIdx = UInt(BackendConfig.storeBufferIdxWidth)
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
  // val storeBufferIdx = Output(UInt(BackendConfig.storeBufferIdxWidth))
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
    val redirect = Output(new RedirectRequest)
    val commitsStoreBuffer = Vec(BackendConfig.maxCommitsNum, new CommitStoreRequest)

    val head = Output(UInt(BackendConfig.robIdxWidth))
    val newException = Output(new NewException)
    val empty = Output(Bool())
    val uncertern = Output(Bool())
    val count = Output(UInt(BackendConfig.robIdxWidth))

    val robEmpty = Output(Bool())

    val interruptInitializing = Input(Bool())
  })
  
  val ctrlIO = IO(new Bundle {
    val flushPipeline = Output(Bool())
  })

  io.newException.valid := false.B
  io.newException.exceptionCode := 0.U
  io.newException.precisePC := 0.U
  io.newException.rawExceptionValue2 := 0.U
  io.newException.interrupt := false.B
  io.newException.csrTag := false.B

  dontTouch(io.uncertern) // 确保时钟中断后，下一次提交的指令有 uncertern

  val entries = RegInit(
    VecInit(Seq.fill(BackendConfig.robSize)(0.U.asTypeOf(new RobEntry)))
  )

  val head = RegInit(0.U(BackendConfig.robIdxWidth))
  val tail = RegInit(0.U(BackendConfig.robIdxWidth))

  io.head := head
  io.empty := head === tail

  io.count := tail - head

  // 默认不发起重定向和冲刷请求

  io.redirect.valid := false.B
  

  ctrlIO.flushPipeline := false.B

  val newsCount = PopCount(newIO.news.map(_.valid))

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
      // entry.storeBufferIdx := DontCare
      entry.storeType := DontCare
      entry.csrTag := false.B
    
      entries(idx) := entry
    }
    newIO.news(i).idx := idx
  }

  tail := tail + newsCount

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
      // jumpTargetError表项的意思是：预测跳转，实际也跳转，但是预测的跳转地址错误
      entry.jumpTargetError := entry.predictJump && complete.jump && entry.jumpTarget =/= complete.jumpTarget
      entry.jumpTarget := complete.jumpTarget

      entry.exception := entry.exception | complete.exception
      // entry.storeBufferIdx := complete.storeBufferIdx
      entry.storeType := complete.storeType
      entry.csrTag := complete.csrTag
      when(complete.exception) {
        entry.exceptionCode := complete.exceptionCode
      }
    }
  }

  // 处理Commit
  val inQueueMask = MaskUtil.GetValidMask(head, tail)

  DebugUtils.Print(cf"RobIdx head: ${head} tail: ${tail} mask: ${Binary(inQueueMask)}")

  val commitValidsOne = Wire(Vec(BackendConfig.maxCommitsNum, Bool()))
  val commitEntry = Wire(Vec(BackendConfig.maxCommitsNum, new RobEntry))

  // 首先找出可能可以被提交的表项，正常情况下，我们可以每周期提交 BackendConfig.maxCommitsNum 条指令
  for (i <- 0 until BackendConfig.maxCommitsNum) {
    val idx = head + i.U
    commitValidsOne(i) := entries(idx).completed && inQueueMask(idx)
    commitEntry(i) := entries(idx)
  }

  // 第一个可能导致不被提交原因是分支预测失败
  // 分支预测失败分为三种
  // 1. 预测跳转，实际不跳转 2. 预测不跳转，实际跳转 3. 跳转地址预测错误
  // commitJumpValid 的意思是没有因为分支预测失败而重定向
  val commitJumpValid =
    VecInit(commitEntry.map(x => !(x.predictJump && !x.realJump || !x.predictJump && x.realJump || x.jumpTargetError)))

  // 第二个可能导致不被提交原因是csr相关指令
  // commitCsrValid 的意思是没有因为csr指令而重定向
  val commitCsrValid = 
    VecInit(commitEntry.map(x => !x.csrTag))

  // 第三个可能导致不被提交原因是执行指令过程中发生了异常，或者中断正在初始化
  // commitExcValid 的意思是没有因为异常而重定向
  val commitExcValid = 
    VecInit(commitEntry.map(x => !x.exception && !io.interruptInitializing))

  // 得到第二版的提交有效信号
  val commitValidsTwo = commitValidsOne
    .zip(commitJumpValid)
    .zip(commitCsrValid)
    .zip(commitExcValid)
    .map {
      case (((valid, jumpValid), csrValid), excValid) => {
        valid && jumpValid && csrValid && excValid
      }
    }

  // 最后考虑，只要同一批次提交的指令中有一个不被提交，后面的指令都不会被提交
  // 得到第3版，以及最终提交有效信号
  val commitValidsThree = commitValidsTwo.scan(true.B)(_ && _).tail
  val commitValidsFinal = WireInit(VecInit(commitValidsThree))

  // 并找到第一个不被正常提交的指令
  val allValid = commitValidsThree.reduce(_ && _)
  val firstInvalidIdx = PriorityEncoder(commitValidsThree.map(!_))
  val invalidEntry = commitEntry(firstInvalidIdx)
  
  io.redirect.target := 0x10000001L.U
  io.newException.precisePC := entries(head - 1.U).jumpTarget
  val afterInterrupt = RegInit(false.B)
  
  /* ================ 重定向逻辑 ================ */
  // 只要有一个指令的commitValidsFinal为False，就需要重定向并冲刷流水线
  when(!allValid && commitValidsOne(firstInvalidIdx)) {
    io.redirect.valid := true.B
    ctrlIO.flushPipeline := true.B
    
    // 下面分别讨论三种情况，注意优先级：csr > exc > predict mistake
    when(!commitCsrValid(firstInvalidIdx)) {
      // 提交执行csr指令
      DebugUtils.Print(cf"RobIdx CSR Instruction: PC = 0x${invalidEntry.vaddr}%x")
      io.newException.valid := true.B
      io.newException.exceptionCode := 0.U
      io.newException.precisePC := invalidEntry.vaddr // 参考蜂鸟p223，加4由软件处理
      io.newException.rawExceptionValue2 := 0.U
      io.newException.csrTag := invalidEntry.csrTag
      commitValidsFinal(firstInvalidIdx) := true.B
      // io.redirect.valid := false.B
      // ctrlIO.flushPipeline := true.B
    }.elsewhen(!commitExcValid(firstInvalidIdx)) {
      io.newException.valid := true.B
      io.newException.exceptionCode := invalidEntry.exceptionCode
      io.newException.precisePC := invalidEntry.vaddr
      io.newException.csrTag := invalidEntry.csrTag
      io.newException.interrupt := !invalidEntry.exception

      afterInterrupt := io.newException.interrupt

      // 使用jumpTarget充当mtval
      io.newException.rawExceptionValue2 := invalidEntry.jumpTarget
      if (DebugConfig.printException) {
        DebugUtils.Print("=== Exception ===")
        DebugUtils.Print(cf"Commit Exception ${invalidEntry.exceptionCode}")
        DebugUtils.Print("=== END ===")
      }
      commitValidsFinal(firstInvalidIdx) := ExceptionCode.CheckCommit(invalidEntry.exceptionCode)

    }.elsewhen(!commitJumpValid(firstInvalidIdx)) {
      // 分支预测失败
      
      commitValidsFinal(firstInvalidIdx) := true.B
      when(invalidEntry.jumpTargetError) {
        // 3. 跳转地址预测错误
        DebugUtils.Print(cf"RobIdx JumpTarget Error, New Target 0x${invalidEntry.jumpTarget}%x")
        io.redirect.target := invalidEntry.jumpTarget
      }.otherwise {
        // 1. 预测跳转，实际不跳转 2. 预测不跳转，实际跳转
        DebugUtils.Print(cf"RobIdx Jump Error, New Jump ${invalidEntry.realJump} 0x${io.redirect.target}%x")
        assert(invalidEntry.realJump =/= invalidEntry.predictJump)
        io.redirect.target := Mux(
          invalidEntry.realJump,
          invalidEntry.jumpTarget,
          invalidEntry.vaddr + 4.U
        )
      } 
    }
  }

  io.robEmpty := head === tail

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
      // out.idx := entry.storeBufferIdx
      out.valid := (valid && (entry.storeType === STORE_RAM || entry.storeType === STORE_MMIO))
    }
  }

  
  when (commitValidsFinal(0)) {
    afterInterrupt := false.B
  }

  io.uncertern := (commitValidsFinal.zip(commitEntry).map{
    case (valid, entry) => {
      valid && (entry.storeType === STORE_MMIO || entry.storeType === LOAD_MMIO) 
    }
  }.reduce(_ || _)) || afterInterrupt

  head := head + PopCount(commitValidsFinal)

  val flush = ctrlIO.flushPipeline
  
  val recover = RegInit(0.U(3.W))

  when (recover =/= 0.U) {
    tail := head
    head := head
    recover := recover >> 1
  } 
  when (flush) {
    recover := "b111".U
    tail := head
    head := head
  }

  if (DebugConfig.printRob) {
    DebugUtils.Print(cf"RobIdx head: ${head} tail: ${tail} count: ${io.count}")
    when(head =/= tail) {
      DebugUtils.Print("=== ROB ===")
      DebugUtils.Print(cf"IDX | OVER | Jv | vaddr | store_type | exception")
      for (i <- 0 until BackendConfig.robSize) {
        val idx = head + i.U
        when(inQueueMask(idx)) {
          val entry = entries(idx)
          DebugUtils.Print(cf"${idx}  ${entry.completed} ${commitJumpValid(idx)}  0x${entry.vaddr}%x ${entry.storeType} ${entry.exception}")
        }
      }
      // print commitValidsFinal
      // DebugUtils.Print(cf"commitValidsFinal: ${commitValidsFinal.asTypeOf(Vec(BackendConfig.maxCommitsNum, Bool()))}")
      DebugUtils.Print("=== END ===")
    }
  }

  assert(newsCount <= ((BackendConfig.robSize - 1).U - io.count))

  // when (!(newsCount <= ((BackendConfig.robSize - 1).U - io.count))) {
  //   DebugUtils.Print(cf"ERROR: ${newsCount} ${(BackendConfig.robSize - 1).U - io.count} ${io.count}")
  // }
  // assert(PopCount(newIO.news.map(_.valid)) === newIO.newsCount)
}   
