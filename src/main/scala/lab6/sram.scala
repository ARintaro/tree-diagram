package core

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog



class SramController extends Module {
  val io = IO(new Bundle {
    // Self Interface
    val cyc = Input(Bool())
    val stb = Input(Bool())
    val ack = Output(Bool())
    val addr = Input(Bits(GlobalConst.ADDR_WIDTH))
    val dataIn = Input(Bits(GlobalConst.DATA_WIDTH))
    val dataOut = Output(Bits(GlobalConst.DATA_WIDTH))
    val bytesEnable = Input(Bits(GlobalConst.SRAM_DATA_BYTES_NUM))
    val writeMode = Input(Bool())

    // Sram Interface
    val sramAddr = Output(Bits(GlobalConst.SRAM_ADDR_WIDTH))
    val sramDataBlock = Output(Bool())
    val sramDataInput = Input(Bits(GlobalConst.DATA_WIDTH))
    val sramDataOutput = Output(Bits(GlobalConst.DATA_WIDTH))
    // 低有效
    val sramDisable = Output(Bool())
    val sramReadDisable = Output(Bool())
    val sramWriteDisable = Output(Bool())
    val sramBytesDisable = Output(Bits(GlobalConst.SRAM_DATA_BYTES_NUM))
  })

  val idle :: reading :: writting1 :: writting2 :: Nil = Enum(4)

  val curState = RegInit(idle)
  val resultReg = RegInit(0.U(GlobalConst.DATA_WIDTH))

  io.sramDisable := false.B
  io.sramWriteDisable := true.B
  io.sramReadDisable := true.B
  io.sramDataBlock := false.B
  io.dataOut := 0.U
  io.ack := false.B

  io.sramAddr := io.addr(21, 2)
  io.sramBytesDisable := ~io.bytesEnable
  io.sramDataOutput := 0.U

  switch(curState) {
    is (idle) {
      io.dataOut := resultReg
    
      when (io.stb && io.cyc) {
        // Receive Quest
        
        when (io.writeMode) {
          // Writting
          io.sramDataOutput := io.dataIn
    
          curState := writting1
        } .otherwise {
          // Reading
          io.sramReadDisable := false.B
          io.sramDataBlock := true.B
          curState := reading
        }
      }
    }
    is (reading) {
      // io.ack := true.B
      io.sramDisable := false.B
      io.sramReadDisable := false.B
      io.sramDataBlock := true.B
      
      resultReg := io.sramDataInput
      io.dataOut := io.sramDataInput

      io.ack := true.B
      curState := idle
    }
    is (writting1) {
      io.sramDisable := false.B
      io.sramWriteDisable := false.B
      io.sramDataOutput := io.dataIn

      curState := writting2
    }
    is (writting2) {
      io.sramDisable := false.B
      io.sramDataOutput := io.dataIn

      curState := idle
      io.ack := true.B
    }
  } 
}