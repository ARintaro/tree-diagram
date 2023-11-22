package core

import chisel3._
import chisel3.util._

class CompressedIssueQueue[T <: Data with IssueInstruction](
    gen: T,
    // 队列大小
    size: Int,
    // 每周期的最大压缩数
    maxMove: Int,
    // 在底部仲裁发射的宽度
    issueScanWidth: Int
) extends Module {
  private val sizeIndexWidth = log2Ceil(size).W

  val io = IO(new Bundle {
    // 从底部仲裁发射的指令
    val issue = Decoupled(gen)
    // 从顶部入队的指令
    val enq = Flipped(Decoupled(gen))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val ram = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))
  val valid = RegInit(0.U(size.W))

  val full = valid === (-1.S(size.W)).asUInt

  io.enq.ready := !full

  val busy = BackendUtils.GetBusy()

  val ready = VecInit(ram.map(_.checkReady(busy))).asUInt & valid
  val scanReady = ready(issueScanWidth - 1, 0)

  val issueSucc = scanReady.orR && io.issue.ready

  val issueIdx = PriorityEncoder(scanReady)

  io.issue.valid := issueSucc
  io.issue.bits := ram(issueIdx)

  val firstEmptyIdx = PriorityEncoder(!valid)
  val doEnq = io.enq.valid && io.enq.ready

  val moveMask = ~MaskUtil.GetPrefixMask(size)(issueIdx)

  when(ctrlIO.flush) {
    valid := 0.U
    io.enq.ready := false.B
  }.otherwise {

    when(issueSucc) {
      for (i <- 0 until size) {
        when(moveMask(i)) {
          ram(i) := ram(i + 1)
        }
      }
    }

    when(doEnq) {
      val enqIdx = Mux(issueSucc, firstEmptyIdx - 1.U, firstEmptyIdx)
      ram(enqIdx) := io.enq.bits
    }

    when(doEnq && !issueSucc) {
      valid := (valid << 1) | 1.U
    }.elsewhen(!doEnq && issueSucc) {
      valid := valid >> 1
    }

  }
}

class FifoIssueQueue[T <: Data with IssueInstruction](
    gen: T,
    size: Int,
    enqPort: Int
) {
  val io = IO(new Bundle {
    val issue = Decoupled(gen)
    val enq = Vec(enqPort, Flipped(Decoupled(gen)))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Bool()
  })

  val queue = Module(new CircularQueue(gen, size, enqPort, 1, "IssueQueue"))

  queue.ctrlIO.flush := ctrlIO.flush
  queue.io.enq <> io.enq
  queue.io.deq(0) <> io.issue
}

class IntInstruction
    extends Bundle
    with InstructionConstants
    with IssueInstruction {
  val robIdx = UInt(BackendConfig.robIdxWidth)
  val prs1 = UInt(BackendConfig.pregIdxWidth)
  val prs2 = UInt(BackendConfig.pregIdxWidth)
  val prd = UInt(BackendConfig.pregIdxWidth)

  // TODO : 立即数压缩
  val imm = UInt(32.W)

  // 操作数选择
  val selOP1 = UInt(OP1_WIDTH)
  val selOP2 = UInt(OP2_WIDTH)
  // 写入目的寄存器
  val writeRd = Bool()
  val bruType = UInt(BRU_WIDTH)
  val aluType = UInt(ALU_WIDTH)

  override def checkReady(busy: UInt): Bool = {
    return (selOP1 =/= OP1_RS1 || !busy(prs1)) && (selOP2 =/= OP2_RS2 || !busy(
      prs2
    ))
  }
}

class MemoryInstruction
    extends Bundle
    with InstructionConstants
    with IssueInstruction {

  val robIdx = UInt(BackendConfig.robIdxWidth)
  val prs = UInt(BackendConfig.pregIdxWidth)
  val prd = UInt(BackendConfig.pregIdxWidth)

  // TODO : 立即数压缩
  val imm = UInt(32.W)

  val memType = Bool() // true: store, false: load
  val memLen = UInt(MEM_LEN_TYPE)

  override def checkReady(busy: UInt): Bool = {
    return !busy(prs)
  }

}
