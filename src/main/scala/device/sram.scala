package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class ExternalSramInterface extends Bundle {
  val addr = Output(UInt(SramConfig.ADDR_WIDTH))
  val dataRead = Input(UInt(SramConfig.DATA_WIDTH))
  val dataWrite = Output(UInt(SramConfig.DATA_WIDTH))
  val readDisable = Output(Bool())
  val writeDisable = Output(Bool())
  val bytesDisable = Output(Bits(SramConfig.DATA_BYTES_NUM))
}

class ExternalSram(name : String) extends Module {
  val io = IO(Flipped(new ExternalSramInterface))

  BoringUtils.addSource(io.addr, f"${name}_addr")
  BoringUtils.addSink(io.dataRead, f"${name}_dataRead")
  BoringUtils.addSource(io.dataWrite, f"${name}_dataWrite")
  BoringUtils.addSource(io.readDisable, f"${name}_readDisable")
  BoringUtils.addSource(io.writeDisable, f"${name}_writeDisable")
  BoringUtils.addSource(io.bytesDisable, f"${name}_bytesDisable")
}

class Sram(name : String) extends Module {
  val busIO = IO(new BusSlaveInterface)

  val externSram = Module(new ExternalSram(name))

  val idle :: reading :: writting :: Nil = Enum(3)
  var state = RegInit(idle)
  var dataReg = RegInit(0.U)

  externSram.io.dataWrite := 0.U
  externSram.io.readDisable := true.B
  externSram.io.writeDisable := true.B
  externSram.io.bytesDisable := DontCare

  externSram.io.addr := busIO.addr(20, 0)
  externSram.io.bytesDisable := ~busIO.dataBytesSelect

  busIO.ack := false.B
  busIO.dataRead := dataReg

  switch(state) {
    is (idle) {
      
      when (busIO.stb) {
        // 收到请求
        when (busIO.dataMode) {
          // write
          externSram.io.dataWrite := busIO.dataWrite
          externSram.io.writeDisable := false.B
          state := writting
        } .otherwise {
          // read
          externSram.io.readDisable := false.B
          state := reading
        }
      }
    }
    is (reading) {
      externSram.io.readDisable := false.B
      busIO.ack := true.B
      dataReg := externSram.io.dataRead

      state := idle
    }
    is (writting) {
      busIO.ack := true.B
      externSram.io.dataWrite := busIO.dataWrite

      state := idle
    }
  }
}

class SramWithArbiter(name : String, inputNum : Int) extends Module {
  

  val io = IO(new Bundle {
    val masters = Vec(inputNum, BusSlaveInterface())
  })

  val sram = Module(new Sram(name))

  val busy = RegInit(false.B)

  val lastReq = Reg(new Bundle {
    val master = UInt(log2Ceil(inputNum).W)
    val addr = UInt(SramConfig.ADDR_WIDTH)
    val dataMode = Bool()
    val dataWrite = UInt(BusConfig.DATA_WIDTH)
    val dataBytesSelect = UInt(BusConfig.DATA_BYTES_NUM)
    val dataRead = UInt(BusConfig.DATA_WIDTH)
  })

  for (i <- 0 until inputNum) {
    io.masters(i).ack := false.B
    io.masters(i).dataRead := sram.busIO.dataRead
  }

  when (busy) {
    sram.busIO.stb := true.B
    sram.busIO.addr := lastReq.addr
    sram.busIO.dataBytesSelect := lastReq.dataBytesSelect
    sram.busIO.dataMode := lastReq.dataMode
    sram.busIO.dataWrite := lastReq.dataWrite

    when (sram.busIO.ack) {
      busy := false.B
      io.masters(lastReq.master).ack := true.B
    }

  } .otherwise {
    val anyStb = io.masters.map(_.stb).reduce(_ || _)

    when (anyStb) {
      val arbiter = Module(new Arbiter(new EmptyBundle, inputNum))
      arbiter.io.out.ready := true.B

      io.masters.map(_.stb).zipWithIndex.foreach{case (stb, i) => arbiter.io.in(i).valid := stb}
      val chosen = arbiter.io.chosen

      val master = io.masters(chosen)

      lastReq.master := chosen
      lastReq.addr := master.addr
      lastReq.dataMode := master.dataMode
      lastReq.dataWrite := master.dataWrite
      lastReq.dataRead := master.dataRead
      lastReq.dataBytesSelect := master.dataBytesSelect

      sram.busIO.stb := true.B
      sram.busIO.addr := master.addr
      sram.busIO.dataBytesSelect := master.dataBytesSelect
      sram.busIO.dataMode := master.dataMode
      sram.busIO.dataWrite := master.dataWrite

      busy := true.B
    } .otherwise {
      sram.busIO.stb := false.B
      sram.busIO.addr := DontCare
      sram.busIO.dataBytesSelect := DontCare
      sram.busIO.dataMode := DontCare
      sram.busIO.dataWrite := DontCare      
    }

  }
  

  require (inputNum >= 2)
}

object SramUtils {
  
  // TODO : 反射
  def AddExternalSram(io : ExternalSramInterface, name : String) : Unit = {
    BoringUtils.addSink(io.addr, f"${name}_addr")
    BoringUtils.addSource(io.dataRead, f"${name}_dataRead")
    BoringUtils.addSink(io.dataWrite, f"${name}_dataWrite")
    BoringUtils.addSink(io.readDisable, f"${name}_readDisable")
    BoringUtils.addSink(io.writeDisable, f"${name}_writeDisable")
    BoringUtils.addSink(io.bytesDisable, f"${name}_bytesDisable")
  }

  
}