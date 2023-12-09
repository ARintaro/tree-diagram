package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class Backend extends Module {
  val io = IO(new Bundle {
    val in = Vec(FrontendConfig.decoderNum, Input(new DecodedInstruction))

    val renameDone = Output(Bool())
    val robRedirect = Output(new RedirectRequest)
    val excRedirect = Output(new RedirectRequest)

    val devBus = Vec(2, BusMasterInterface())
    val robCount = Output(UInt(BackendConfig.robIdxWidth))
  })

  val ctrlIO = IO(new Bundle {
    val flushPipeline = Output(Bool())
    val clearICache = Output(Bool())
  })


  val renameTable = Module(new RenameTable)
  val renameUnit = Module(new RenameUnit)
  val rob = Module(new ReorderBuffer)
  val dispatch = Module(new DispatchUnit)
  val registers = Module(
    new PhysicalRegisterFile(
      BackendConfig.pipelineNum + 1,
      BackendConfig.pipelineNum + 1
    )
  )
  val intPipes =
    (0 until BackendConfig.intPipelineNum).map(x => Module(new IntPipeline(x)))

  val intQueues = (0 until BackendConfig.intPipelineNum).map(x => {
    Module(
      new CompressedIssueQueue(
        new IntInstruction,
        BackendConfig.intQueueSize,
        BackendConfig.intQueueScanWidth
      )
    )
  })

  val memPipe = Module(new MemoryPipeline(BackendConfig.intPipelineNum))

  val memQueue = Module(
    new FifoCompressedIssueQueue(
      gen=new MemoryInstruction,
      size=BackendConfig.memQueueSize,
      enqPort=FrontendConfig.decoderNum
    )
  )

  val dataCache = Module(new DataCache)
  val storeBuffer = Module(new CompressedStoreBuffer(findPortNum = 1)) // TODO : 1个findPortNum够用吗
  
  

  if(DebugConfig.printIssue) {
    for(i <- 0 until BackendConfig.intPipelineNum) {
      when(intQueues(i).io.issue.valid && intQueues(i).io.issue.ready) {
        DebugUtils.Print(
          cf"intIntruction issued, robidx${intQueues(i).io.issue.bits.robIdx}"
        )
      }
    }
  }
  // Rename Table
  

  // Rename Unit
  renameUnit.renameTableIO <> renameTable.io.renames
  renameUnit.robIO <> rob.newIO
  
  renameUnit.io.in := io.in
  renameUnit.io.nextDone := dispatch.io.done
  io.renameDone := renameUnit.io.done

  // Dispatch Unit
  dispatch.io.in := renameUnit.io.out
  dispatch.io.mem.foreach(x => {
    x.ready := true.B
  })
  dispatch.io.ints.zip(intQueues.map(_.io.enq)).foreach {
    case (x, y) => {
      x <> y
    }
  }
  dispatch.io.mem <> memQueue.io.enq  

  // Int Pipeline
  for (i <- 0 until BackendConfig.intPipelineNum) {
    val pipe = intPipes(i)
    pipe.io.in <> intQueues(i).io.issue
    pipe.io.robComplete <> rob.completeIO(i)
    pipe.io.pcRead <> rob.readPcIO(i)
    pipe.io.regRead <> registers.io.reads(i)
    pipe.io.regWrite <> registers.io.writes(i)
  }

  // MEM Pipeline
  memPipe.io.in <> memQueue.io.issue
  memPipe.io.robComplete <> rob.completeIO(BackendConfig.intPipelineNum) // 这里的robComplete是给memPipe用的,下标是BackendConfig.intPipelineNum
  memPipe.io.regRead <> registers.io.reads(BackendConfig.intPipelineNum)
  memPipe.io.regWrite <> registers.io.writes(BackendConfig.intPipelineNum)
  memPipe.io.findStore <> storeBuffer.io.finds(0)
  memPipe.io.newStore <> storeBuffer.io.news
  memPipe.io.bus <> io.devBus(0)
  memPipe.io.robHead := rob.io.head
  // memPipe.io.robEmpty := rob.io.empty
  memPipe.io.cacheResult := dataCache.f2_io.entry
  dataCache.f1_io.vaddr := memPipe.io.cacheFindVaddr
  dataCache.f2_io.write := memPipe.io.cacheWrite

  

  // ROB
  rob.commitsIO <> renameTable.io.commits
  rob.io.commitsStoreBuffer <> storeBuffer.io.commits
  io.robCount := rob.io.count
  

  if (DebugConfig.printFlush) {
    when(rob.ctrlIO.flushPipeline) {
      DebugUtils.Print("Flush !!")
    }
  }

  if (DebugConfig.printRedirect) {
    when (io.robRedirect.valid) {
      DebugUtils.Print(cf"ROB Redirect : 0x${io.robRedirect.target}%x")
    }

    when (io.exeRedirect.valid) {
      DebugUtils.Print(cf"EXEU Redirect : 0x${io.robRedirect.target}%x")
    }
  }


  // store buffer
  storeBuffer.busIO <> io.devBus(1)
  storeBuffer.io.cache <> dataCache.storeIO
  

  // exception Unit
  val excu = Module(new ExceptionUnit)
  excu.io.exc <> rob.io.newException
  excu.io.reference <> dispatch.io.csr
  excu.io.rawExceptionValue1 := 0.U // TODO: 接到前端
  excu.io.regRead <> registers.io.reads(BackendConfig.intPipelineNum + 1)(0)
  registers.io.reads(BackendConfig.intPipelineNum + 1)(1).id := 0.U
  excu.io.regWrite <> registers.io.writes(BackendConfig.intPipelineNum + 1)
  

  // global flush logic
  val doFlush = RegNext(excu.ctrlIO.flushPipeline || rob.ctrlIO.flushPipeline)
  val flushDelay = RegNext(doFlush)

  ctrlIO.flushPipeline := doFlush
  io.robRedirect := RegNext(rob.io.redirect)
  io.excRedirect := RegNext(excu.io.redirect)

  renameTable.ctrlIO.recover := flushDelay
  renameUnit.ctrlIO.flush := flushDelay
  memQueue.ctrlIO.flush := flushDelay
  storeBuffer.ctrlIO.flush := flushDelay
  dispatch.ctrlIO.flush := flushDelay
  intQueues.foreach(_.ctrlIO.flush := flushDelay)
  for (i <- 0 until BackendConfig.intPipelineNum) {
    intPipes(i).ctrlIO.flush := flushDelay
  }
  memPipe.ctrlIO.flush := flushDelay

  

  // debug: print flush
  if (DebugConfig.printFlush) {
    when(doFlush) {
      DebugUtils.Print("Backend Flush !!")
    }
    when(excu.ctrlIO.flushPipeline) {
      DebugUtils.Print("Backend Flush by Exception !!")
    }
    when(rob.ctrlIO.flushPipeline) {
      DebugUtils.Print("Backend Flush by ROB !!")
    }
    when(flushDelay) {
     DebugUtils.Print("Backend Flush Delay !!")
   }
  }

  // FENCEI signal 
  ctrlIO.clearICache := excu.ctrlIO.clearICache
}
