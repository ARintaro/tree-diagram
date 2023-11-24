package core

import chisel3._
import chisel3.util._

class Backend extends Module {
  val io = IO(new Bundle {
    val in = Vec(FrontendConfig.decoderNum, Input(new DecodedInstruction))

    val renameDone = Output(Bool())
    val robRedirect = Valid(UInt(BusConfig.ADDR_WIDTH))
  })

  val ctrlIO = IO(new Bundle {
    val flushPipeline = Output(Bool())
  })

  val flush = Wire(Bool())

  ctrlIO.flushPipeline := flush

  val renameTable = Module(new RenameTable)
  val renameUnit = Module(new RenameUnit)
  val rob = Module(new ReorderBuffer)
  val dispatch = Module(new DispatchUnit)
  val registers = Module(
    new PhysicalRegisterFile(
      BackendConfig.intPipelineNum ,
      BackendConfig.intPipelineNum
    )
  )
  val intPipes =
    (0 until BackendConfig.intPipelineNum).map(x => Module(new IntPipeline(x)))

  val intQueues = (0 until BackendConfig.intPipelineNum).map(x => {
    Module(new CompressedIssueQueue(new IntInstruction, BackendConfig.intQueueSize, BackendConfig.intQueueScanWidth))
  })

  // Rename Table
  renameTable.ctrlIO.recover := flush

  // Rename Unit
  renameUnit.renameTableIO <> renameTable.io.renames
  renameUnit.robIO <> rob.newIO
  renameUnit.ctrlIO.flush := flush
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

  // Int Queues
  intQueues.foreach(_.ctrlIO.flush := flush)


  // Int Pipeline
  for (i <- 0 until BackendConfig.intPipelineNum) {
    val pipe = intPipes(i)
    pipe.io.in <> intQueues(i).io.issue
    pipe.io.robComplete <> rob.completeIO(i)
    pipe.io.pcRead <> rob.readPcIO(i)
    pipe.io.regRead <> registers.io.reads(i)
    pipe.io.regWrite <> registers.io.writes(i)
    pipe.ctrlIO.flush := flush
  }

  // ROB
  rob.commitsIO <> renameTable.io.commits
  io.robRedirect <> rob.io.redirect
  flush := rob.ctrlIO.flushPipeline
}
