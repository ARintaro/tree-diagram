package core

import chisel3._
import chisel3.util._

class RobEntry extends Bundle {
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
  val exceptionCode = UInt(InsConfig.EXECEPTION_WIDTH)

  // TODO : 访存相关记录

}

class NewRobRequest extends Bundle {
  val valid = Input(Bool())

  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  val writeRd = Input(Bool())
  val rdLidx = Input(UInt(5.W))
  val rdPidx = Input(UInt(BackendConfig.pregIdxWidth))
  val predictJump = Input(Bool())
  val predictJumpTarget = Input(UInt(BusConfig.ADDR_WIDTH))
  val exception = Input(Bool())
  val exceptionCode = Input(UInt(InsConfig.EXECEPTION_WIDTH))

  val idx = Output(UInt(BackendConfig.robSize.W))
}

class RobNewIO extends Bundle {
  val news = Vec(FrontendConfig.decoderNum, new NewRobRequest)
  val newsCount = Input(UInt((log2Ceil(FrontendConfig.decoderNum)).W))

  val restSize = Output(UInt((log2Ceil(BackendConfig.robSize)).W))
}

class RobReadPcRequest extends Bundle {
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
}

class RobCompleteRequest extends Bundle {
  val valid = Output(Bool())
  val robIdx = Output(UInt(BackendConfig.robIdxWidth))
  val jump = Output(Bool())
  val jumpTarget = Output(UInt(BusConfig.ADDR_WIDTH))
  val exception = Output(Bool())
  val exceptionCode = Output(UInt(InsConfig.EXECEPTION_WIDTH))
}

class ReorderBuffer extends Module {
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
  })


  val ctrlIO = IO(new Bundle {
    val flushPipeline = Output(Bool())
  })

  val entries = RegInit(
    VecInit(Seq.fill(BackendConfig.robSize)(0.U.asTypeOf(new RobEntry)))
  )

  val head = RegInit(0.U(BackendConfig.robIdxWidth))
  val tail = RegInit(0.U(BackendConfig.robIdxWidth))

  val count = tail - head

  newIO.restSize := BackendConfig.robSize.U - count

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

      entries(idx) := entry
    }
    newIO.news(i).idx := idx
  }

  tail := tail + newIO.newsCount

  // 处理流水线写回的complete请求
  for (i <- 0 until BackendConfig.pipelineNum) {
    val complete = completeIO(i)
    val entry = entries(complete.robIdx)
    when(complete.valid) {
      entry.completed := true.B
      entry.realJump := complete.jump
      entry.jumpTargetError := complete.jump && entry.jumpTarget === complete.jumpTarget
      entry.jumpTarget := complete.jumpTarget
      entry.exception := entry.exception | complete.exception
      when(complete.exception) {
        entry.exceptionCode := complete.exceptionCode
      }
    }
  }

  // 处理Commit
  val inQueueMask = MaskUtil.GetValidMask(head, tail)

  val commitBasicValids = Wire(Vec(BackendConfig.maxCommitsNum, Bool()))
  val commitEntry = Wire(Vec(BackendConfig.maxCommitsNum, new RobEntry))

  for (i <- 0 until BackendConfig.maxCommitsNum) {
    val idx = head + i.U
    commitBasicValids(i) := entries(idx).completed && inQueueMask(idx)
    commitEntry(i) := entries(idx)
  }

  val commitJumpValid = commitEntry.map(x => !x.jumpTargetError && x.realJump === x.predictJump)

  val commitValid = commitBasicValids.zip(commitJumpValid).zip(commitEntry).map {
    case ((x, y), z) => x && y && !z.exception
  }.scan(true.B)(_ && _).tail

  // 这里没有用寄存器暂存输出，时序扛不住的话需要加上
  commitsIO.zip(commitValid).zip(commitEntry).foreach {
    case ((out, valid), entry) => {
      out.valid := valid
      out.pregIdx := entry.rdPidx
      out.lregIdx := entry.rdLidx
    }
  }

  val allValid = commitValid.reduce(_ || _)
  val firstInvalidIdx = PriorityEncoder(commitValid.map(!_))

  val invalidEntry = commitEntry(firstInvalidIdx)
  when (!allValid && commitBasicValids(firstInvalidIdx)) {
    io.redirect.valid := true.B
    ctrlIO.flushPipeline := true.B

    when (invalidEntry.exception) {
      // TODO : 异常跳转地址
      assert(false.B)
    }.otherwise {
      // 分支预测失败
      when (invalidEntry.jumpTargetError) {
        // 跳转地址错误
        io.redirect.bits := invalidEntry.jumpTarget
      } .otherwise {
        // 是否跳转错误
        assert(invalidEntry.realJump =/= invalidEntry.predictJump)
        io.redirect.bits := Mux(invalidEntry.realJump, invalidEntry.jumpTarget, invalidEntry.vaddr + 4.U)
      }
    }
  }


  val flush = ctrlIO.flushPipeline
  when (flush) {
    head := 0.U
    tail := 0.U
  }



  assert(newIO.newsCount < newIO.restSize)
  assert(PopCount(newIO.news.map(_.valid)) === newIO.newsCount)
}
