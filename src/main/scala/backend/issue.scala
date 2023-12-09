package core

import chisel3._
import chisel3.util._

class CompressedIssueQueue[T <: Data with IssueInstruction](
    gen: T,
    // 队列大小
    size: Int,
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

  val full = valid(size - 1)

  io.enq.ready := !full

  val busy = BackendUtils.GetBusy()

  val ready = VecInit(ram.map(_.checkReady(busy))).asUInt & valid
  val scanReady = ready(issueScanWidth - 1, 0)

  val issueSucc = scanReady.orR && io.issue.ready

  val issueIdx = PriorityEncoder(scanReady)

  io.issue.valid := issueSucc
  io.issue.bits := ram(issueIdx)

  val firstEmptyIdx = PriorityEncoder(~valid)
  val doEnq = io.enq.valid && io.enq.ready

  val moveMask = ~MaskUtil.GetPrefixMask(size)(issueIdx)

  when(issueSucc) {
    for (i <- 0 until size - 1) {
      when(moveMask(i)) {
        ram(i) := ram(i + 1)
      }
    }
  }

  if (DebugConfig.printIssue) {
    when (doEnq) {
      DebugUtils.Print(cf"int compressed enq ${io.enq.bits}")
    }
    DebugUtils.Print(cf"==== Int Queues ====")
    for (i <- 0 until size) {
      when(valid(i)) {
        DebugUtils.Print(cf"${i} ${ram(i)}")
      }
    }
    DebugUtils.Print(cf"==== Int Queues  END ====")
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

  when(ctrlIO.flush) {
    valid := 0.U
  }
}

class FifoIssueQueue[T <: Data with IssueInstruction](
    gen: T,
    size: Int,
    enqPort: Int
)extends Module {
  val io = IO(new Bundle {
    val issue = Decoupled(gen)
    val enq = Vec(enqPort, Flipped(Decoupled(gen)))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val queue = Module(new CircularQueue(gen, size, enqPort, 1, "IssueQueue"))

  queue.ctrlIO.flush := ctrlIO.flush
  queue.io.enq <> io.enq

  val front = queue.io.deq(0)
  val busy = BackendUtils.GetBusy()

  val dataReady = front.bits.checkReady(busy)

  io.issue.valid := front.valid && dataReady
  io.issue.bits := front.bits
  front.ready := io.issue.ready && dataReady
}

class FifoCompressedIssueQueue[T <: Data with IssueInstruction](
    gen: T,
    size: Int,
    enqPort: Int
)extends Module {
  val io = IO(new Bundle {
    val issue = Decoupled(gen)
    val enq = Vec(enqPort, Flipped(Decoupled(gen)))
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })
  
  val ram = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))
  val valid = RegInit(0.U(size.W))

  // TODO : 更好的ready
  io.enq.foreach(_.ready := !valid(size - enqPort)) 

  val busy = BackendUtils.GetBusy()

  val front = ram(0)

  io.issue.valid := valid(0) && front.checkReady(busy)
  io.issue.bits := front

  val enqIdx = Wire(UInt(log2Ceil(size).W))

  val enqCount = PopCount(io.enq.map(_.valid))
  val firstEmptyIdx = PriorityEncoder(~valid)

  val newValid = Wire(UInt(size.W))

  when (io.issue.ready && io.issue.valid) {
    for (i <- 0 until size - 1) {
      ram(i) := ram(i + 1)
    }
    enqIdx := firstEmptyIdx - 1.U

    when (enqCount === 0.U) {
      newValid := valid >> 1
    } .otherwise {
      val move = enqCount - 1.U
      newValid := (valid << move) | MaskUtil.GetPrefixMask(enqPort)(move)
    }

  } .otherwise {
    enqIdx := firstEmptyIdx

    newValid := (valid << enqCount) | MaskUtil.GetPrefixMask(enqPort)(enqCount)
  }

  for (i <- 0 until enqPort) {
    when (io.enq(i).valid && io.enq(i).ready) {
      ram(enqIdx + i.U) := io.enq(i).bits
      
      if (DebugConfig.printIssue) {
        DebugUtils.Print(cf"fifo compressed enq ${i} ${io.enq(i).bits}")
      }
    }
  }

  if (DebugConfig.printIssue) {
    for (i <- 0 until size) {
      when(valid(i)) {
        DebugUtils.Print(cf"fifo compressed queue entry ${i} ${ram(i)}")
      }
    }
  }

  assert((PopCount(valid) + enqCount) - Mux(io.issue.ready && io.issue.valid, 1.U, 0.U)  === PopCount(newValid))

  when (ctrlIO.flush) {
    valid := 0.U
  } .otherwise {
    valid := newValid
  }
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

  // csr related instructions
  val csrTag = Bool()

  override def checkReady(busy: UInt): Bool = {
    return MuxLookup(bruType, !busy(prs1) && !busy(prs2)) (
      Seq(
        BRU_NONE -> ((selOP1 =/= OP1_RS1 || !busy(prs1)) && (selOP2 =/= OP2_RS2 || !busy(prs2))),
        BRU_JALR -> !busy(prs1),
      )
    ) 
  }
}

class MemoryInstruction
    extends Bundle
    with InstructionConstants
    with IssueInstruction {

  val robIdx = UInt(BackendConfig.robIdxWidth)
  val prs1 = UInt(BackendConfig.pregIdxWidth)
  val prd_or_prs2 = UInt(BackendConfig.pregIdxWidth)
  // lb rd, rs, imm 
  // sw rs1, rs2, imm

  // TODO : 立即数压缩
  val imm = UInt(32.W)

  val memType = Bool() // true: store, false: load
  val memLen = UInt(MEM_LEN_WIDTH)

  val extType = Bool()

  override def checkReady(busy: UInt): Bool = {
    return Mux(memType, !busy(prs1) && !busy(prd_or_prs2), !busy(prs1))
  }

  def getValue(addr: UInt, value: UInt): UInt = {
    val rawValue = MuxLookup(memLen, 0.U)(
      Seq(
        MEM_BYTE -> value(7, 0),
        MEM_HALF -> value(15, 0),
        MEM_WORD -> value
      )
    )
    MuxLookup(addr(1, 0), 0.U)(
      Seq(
        "b00".U -> rawValue,
        "b01".U -> (rawValue << 8.U),
        "b10".U -> (rawValue << 16.U),
        "b11".U -> (rawValue << 24.U)
      )
    )
  }

  def getBytes(addr: UInt): UInt = {
    val rawBytes = MuxLookup(memLen, 0.U)(
      Seq(
        MEM_BYTE -> "b0001".U,
        MEM_HALF -> "b0011".U,
        MEM_WORD -> "b1111".U
      )
    )
    return (rawBytes << addr(1, 0))
  }

}
