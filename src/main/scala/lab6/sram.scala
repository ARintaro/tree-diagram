package core

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog



class SramController(name : String) extends Module {
  val busIO = IO(new BusSlaveInterface)

  val externSram = Module(new ExternalSram(name))

  val idle :: reading :: writting1 :: writting2 :: done :: Nil = Enum(5)

  val curState = RegInit(idle)
  val resultReg = RegInit(0.U(GlobalConst.DATA_WIDTH))

  externSram.io.writeDisable := true.B
  externSram.io.readDisable := true.B
  externSram.io.dataWrite := 0.U

  busIO.mmio := false.B
  busIO.ack := false.B
  busIO.dataRead := resultReg

  externSram.io.addr := busIO.addr(21, 2)
  externSram.io.bytesDisable := ~busIO.dataBytesSelect

  switch(curState) {
    is (idle) {
      when (busIO.stb) {
        // Receive Quest
        
        when (busIO.dataMode) {
          // Writting
          externSram.io.dataWrite := busIO.dataWrite
    
          curState := writting1
        } .otherwise {
          // Reading
          externSram.io.bytesDisable := "b0000".U
          externSram.io.readDisable := false.B
          curState := reading
        }
      }
    }
    is (reading) {
      // io.ack := true.B
      externSram.io.readDisable := false.B

      resultReg := externSram.io.dataRead
      busIO.dataRead := externSram.io.dataRead
      externSram.io.bytesDisable := "b0000".U

      busIO.ack := true.B
      curState := done
    }
    is (writting1) {
      externSram.io.writeDisable := false.B
      externSram.io.dataWrite := busIO.dataWrite

      curState := writting2
    }
    is (writting2) {
      externSram.io.dataWrite := busIO.dataWrite

      curState := done
      busIO.ack := true.B
    }
    is(done) {
      curState := idle
    }
  } 
}