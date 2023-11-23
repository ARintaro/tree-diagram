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

  val renameTable = Module(new RenameTable)
  val renameUnit = Module(new RenameUnit)
  val rob = Module(new ReorderBuffer)
  val dispatch = Module(new DispatchUnit)




  // Rename Unit
  renameUnit.renameTableIO <> renameTable.io.renames
  renameUnit.robIO <> rob.newIO
  renameUnit.ctrlIO.flush := flush
  renameUnit.io.in := io.in
  io.renameDone := renameUnit.io.done



}