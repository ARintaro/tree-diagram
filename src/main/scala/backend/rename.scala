package core

import chisel3._
import chisel3.util._
import svsim.Backend
import chisel3.util.experimental.BoringUtils

class RenameTableEntry extends Bundle {
  val valid = Bool()
  val pregIdx = UInt(BackendConfig.pregIdxWidth)
}

class NewPregRequest extends Bundle {
  // 注意写x0的话这里的valid应该设成false
  val valid = Output(Bool())
  val lregIdx = Output(UInt(5.W))

  // 返回的物理寄存器下标
  val pregIdx = Input(UInt(BackendConfig.pregIdxWidth))
}

class FindPregRequest extends Bundle {
  val lregIdx = Output(UInt(5.W))

  val preg = Input(UInt(BackendConfig.pregIdxWidth))
}

class CommitPregRequest extends Bundle {
  val valid = Output(Bool())

  // commit时
  // 将映射关系写入到art中，让逻辑寄存器对应的上一个物理寄存器重新加入freeList里
  val lregIdx = Output(UInt(5.W))
  val pregIdx = Output(UInt(BackendConfig.pregIdxWidth))
}

class RenameRequests extends Bundle {
  // 重命名请求
  val news = Vec(FrontendConfig.decoderNum, new NewPregRequest)
  val finds = Vec(FrontendConfig.decoderNum, Vec(2, new FindPregRequest))
  // 重命名请求是否成功
  val succ = Input(Bool())
}

class RenameTable extends Module {
  val io = IO(new Bundle {
    val renames = Flipped(new RenameRequests)

    // 提交请求
    val commits = Vec(BackendConfig.maxCommitsNum, Flipped(new CommitPregRequest))
  })

  val ctrlIO = IO(new Bundle {
    val recover = Input(Bool())
  })

  // speculative rename table
  val srt = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new RenameTableEntry))))
  // speculative preg free list
  // 如果为0表示已经被占用
  // 利用-1初始化为全1
  val sfree = RegInit(UIntUtils.GetAllOneWithoutZero(BackendConfig.physicalRegNum))

  val busy = RegInit(0.U(BackendConfig.physicalRegNum.W))

  // arch rename table
  val art = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new RenameTableEntry))))
  // arch preg free list
  val afree = RegInit(UIntUtils.GetAllOneWithoutZero(BackendConfig.physicalRegNum))

  val recovering = RegInit(false.B)

  if (DebugConfig.printBusy) {
    DebugUtils.Print(cf"Busy ${busy.asTypeOf(Vec(BackendConfig.physicalRegNum, Bool()))}")
  }

  if (DebugConfig.printRenameTable) {
    DebugUtils.Print(cf"Sfree ${sfree.asTypeOf(Vec(BackendConfig.physicalRegNum, Bool()))}")
    DebugUtils.Print(cf"Afree ${afree.asTypeOf(Vec(BackendConfig.physicalRegNum, Bool()))}")

    for (i <- 0 until 32) {
      val entry = srt(i)
      when (entry.valid) {
        DebugUtils.Print(cf"Spec Name Entry ${i} -> ${entry.pregIdx}")
      }
    }

    for (i <- 0 until 32) {
      val entry = art(i)
      when (entry.valid) {
        DebugUtils.Print(cf"Arch Name Entry ${i} -> ${entry.pregIdx}")
      }
    }
  }

  when(recovering) {
    // 从art恢复出freeList与srt
    srt := art
    sfree := afree
    io.renames.news.foreach(x => {
      x.pregIdx := DontCare
    })
    io.renames.finds.foreach(x =>
      x.foreach(y => {
        y.preg := DontCare
      })
    )
    io.renames.succ := false.B
    busy := 0.U
    recovering := false.B
    
  }.otherwise {
    // 处理news和finds


    // 分配出去的物理寄存器
    val allocs = WireInit(0.U(BackendConfig.physicalRegNum.W))
    
    {
      // 要么全部失败，要么全部成功
      // 是否成功必须尽快计算，不应影响前序流水段的时序
      
      // 比较sfree中1的个数和news中valid的个数
      
      val validCount = PopCount(io.renames.news.map(_.valid))
      val freeCount = PopCount(sfree)
      val succ = validCount <= freeCount

      // 当前重命名表
      var curRT = Wire(Vec(32, new RenameTableEntry))
      // 当前freeList
      var curFree = Wire(UInt(BackendConfig.physicalRegNum.W))

      curRT := srt
      curFree := sfree

      // 按指令顺序更新
      for (i <- 0 until FrontendConfig.decoderNum) {
        // 先处理Find，避免读写逻辑寄存器相同的情况
        val finds = io.renames.finds(i)
        for (j <- 0 until 2) {
          val entry = curRT(finds(j).lregIdx)
          finds(j).preg := Mux(entry.valid, entry.pregIdx, 0.U)
        }

        val req = io.renames.news(i)
        // 更新目前的失败状态
        // 更新curRT与curFree

        val select = Mux(req.valid, PriorityEncoderOH(curFree), 0.U)
        val selectIdx = OHToUInt(select)

        val newRT = Wire(Vec(32, new RenameTableEntry))
        newRT := curRT
        when(req.valid) {
          newRT(req.lregIdx).valid := true.B
          newRT(req.lregIdx).pregIdx := selectIdx

          if (DebugConfig.printRenameAlloc) {
            DebugUtils.Print(cf"Rename alloc: lregIdx: ${req.lregIdx} -> pregIdx: ${selectIdx}")
          }

        }

        req.pregIdx := selectIdx

        // 注意这里是 "等于号"
        curFree = curFree & ~select
        curRT = newRT
      }


      io.renames.succ := succ
      // 最终结果写回寄存器
      when(succ) {
        srt := curRT
        allocs := sfree & ~curFree
      }
    }

    // 处理commit

    // 回收的物理寄存器
    val recycles = WireInit(0.U(BackendConfig.physicalRegNum.W))

    {
      // 当前重命名表
      var curRT = Wire(Vec(32, new RenameTableEntry))
      // 当前freeList
      var curSFree = Wire(UInt(BackendConfig.physicalRegNum.W))
      var curAFree = Wire(UInt(BackendConfig.physicalRegNum.W))

      curRT := art
      curSFree := sfree
      curAFree := afree

      for (i <- 0 until BackendConfig.maxCommitsNum) {
        val req = io.commits(i)

        val lastEntry = curRT(req.lregIdx)
        val lastPregOH =
          Mux(lastEntry.valid && req.valid, UIntToOH(lastEntry.pregIdx), 0.U)
        val curPregOH = Mux(req.valid, UIntToOH(req.pregIdx), 0.U)

        // 把之前的物理寄存器重新加入SFree
        curSFree = curSFree | lastPregOH
        // 把之前的物理寄存器加入AFree，把现在的物理寄存器从AFree里删掉
        curAFree = (curAFree | lastPregOH) & ~curPregOH

        // 写入新的映射关系
        val newRT = Wire(Vec(32, new RenameTableEntry))
        newRT := curRT
        when(req.valid) {
          newRT(req.lregIdx).valid := true.B
          newRT(req.lregIdx).pregIdx := req.pregIdx

          if (DebugConfig.printRenameFree) {
            when (lastEntry.valid) {
              DebugUtils.Print(cf"Rename commit: lregIdx: ${req.lregIdx} -> pregIdx: ${req.pregIdx}, freePreg: ${lastEntry.pregIdx}")
            }.otherwise {
              DebugUtils.Print(cf"Rename commit: lregIdx: ${req.lregIdx} -> pregIdx: ${req.pregIdx}")
            }
          }
        }

        curRT = newRT
      }

      // 最终结果写回
      art := curRT
      recycles := ~sfree & curSFree
      afree := curAFree
    }

    sfree := sfree & ~allocs | recycles

    // 根据和alloc广播更新Busy
    

    val wakeupHot = BackendUtils.GetWakeupHot()
    
    busy := busy & ~wakeupHot | allocs
    BoringUtils.addSource(busy, "busy")

    assert((wakeupHot & allocs) === 0.U)
    assert((busy & allocs) === 0.U)
    assert((allocs & recycles) === 0.U)
    assert(allocs(0) === 0.U)
    assert(recycles(0) === 0.U)

    // 回滚信号
    // 注意flush拉高的这一周期的commit仍然需要有效
    // 所以使用状态机下一周期开始恢复
    recovering := ctrlIO.recover


  }

}

class RenameUnit extends Module
 with InstructionConstants {
  val io = IO(new Bundle {
    val in = Vec(FrontendConfig.decoderNum, Input(new DecodedInstruction))
    val done = Output(Bool())


    val out = Vec(FrontendConfig.decoderNum, Output(new PipelineInstruction))
    val nextDone = Input(Bool())
  })

  val renameTableIO = IO(new RenameRequests)
  val robIO = IO(Flipped(new RobNewIO))

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val outBuffer = RegInit(VecInit(Seq.fill(FrontendConfig.decoderNum)(0.U.asTypeOf(new PipelineInstruction))))
  io.out <> outBuffer
  
  // 进行寄存器重命名

  // 快速判断是否成功
  // 0. 下一段ready，可以覆盖outBuffer
  // 1. rob中有足够空间
  // 2. RenameTable中可以进行写入寄存器的重命名

  val inCount = PopCount(io.in.map(_.valid))

  // 这里flush了也能写，反正这里flush了，rob肯定会recover了
  val robSucc = robIO.restSize > inCount && io.nextDone
  val succ = robSucc && renameTableIO.succ
  robIO.newsCount := Mux(succ, inCount, 0.U)
  io.done := succ

  for (i <- 0 until FrontendConfig.decoderNum) {
    // 连接renameTable
    renameTableIO.news(i).valid := io.in(i).valid && io.in(i).writeRd && robSucc
    renameTableIO.news(i).lregIdx := io.in(i).rd
    renameTableIO.finds(i)(0).lregIdx := io.in(i).rs1
    renameTableIO.finds(i)(1).lregIdx := io.in(i).rs2

    // 写入rob
    robIO.news(i).valid := io.in(i).valid && succ
    robIO.news(i).vaddr := io.in(i).vaddr
    robIO.news(i).writeRd := io.in(i).writeRd
    robIO.news(i).rdLidx := io.in(i).rd
    robIO.news(i).rdPidx := renameTableIO.news(i).pregIdx
    robIO.news(i).exception := io.in(i).exception
    robIO.news(i).exceptionCode := io.in(i).exceptionCode
    robIO.news(i).predictJump := io.in(i).predictJump
    robIO.news(i).predictJumpTarget := io.in(i).predictTarget

    if (DebugConfig.printRobNew) {
      when (robIO.news(i).valid) {
        var base = cf"Rob New Idx ${robIO.news(i).idx} Inst 0x${io.in(i).inst}%x vaddr 0x${io.in(i).vaddr}%x"
        when (robIO.news(i).writeRd) {
          base += cf"Rd ${renameTableIO.news(i).pregIdx}"
        }
        DebugUtils.Print(base)
      }
    }

  }

  when (succ) {
    for (i <- 0 until FrontendConfig.decoderNum) {
      val ins = Wire(new PipelineInstruction)
      ins.valid := io.in(i).valid
      ins.robIdx := robIO.news(i).idx
      ins.aluType := io.in(i).aluType
      ins.bruType := io.in(i).bruType
      ins.selOP1 := io.in(i).selOP1
      ins.selOP2 := io.in(i).selOP2
      ins.writeRd := io.in(i).writeRd
      ins.unique := io.in(i).unique
      ins.flush := io.in(i).flush
      ins.imm := io.in(i).imm
      ins.iqtType := io.in(i).iqtType
      ins.memLen := io.in(i).memLen
      ins.memType := io.in(i).memType

      ins.prs1 := renameTableIO.finds(i)(0).preg
      ins.prs2 := renameTableIO.finds(i)(1).preg
      ins.prd := renameTableIO.news(i).pregIdx

      if (DebugConfig.printPipeIns) {
        DebugUtils.Print(cf"PipeIns RobIdx : ${ins.robIdx} Sel1:${ins.selOP1} prs1:${ins.prs1} Sel2:${ins.selOP2} prs2:${ins.prs2}")
      }
      
      outBuffer(i) := ins
    }
  } .elsewhen(ctrlIO.flush || io.nextDone) {
    outBuffer.foreach(x => {
      x.valid := false.B
    })
  }

}
