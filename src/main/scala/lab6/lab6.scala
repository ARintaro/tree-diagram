package core

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog
import scala.runtime.Statics

object GlobalConst {
  val DATA_WIDTH = 32.W
  val ADDR_WIDTH = 32.W

  val UART_DATA_WIDTH = 8.W

  val DATA_BYTES_NUM = 4.W

  val SRAM_ADDR_WIDTH = 20.W
  val SRAM_DATA_WIDTH = 32.W

  val SRAM_DATA_BYTES_NUM = 4.W
}

class BusInterface extends Bundle {
  val cyc = Output(Bool())
  val stb = Output(Bool())
  val ack = Input(Bool())
  val addr = Output(UInt(GlobalConst.ADDR_WIDTH))

  val dataBytesSelect = Output(UInt(GlobalConst.DATA_BYTES_NUM))
  // True for write, false for read
  val dataMode = Output(Bool())
  val dataWrite = Output(UInt(GlobalConst.DATA_WIDTH))
  val dataRead = Input(UInt(GlobalConst.DATA_WIDTH))
}

class BusReq extends Bundle {
  val addr = UInt(32.W)

  val dataBytesSelect = UInt(4.W)
  val dataMode = Bool()
  val dataWrite = UInt(32.W)
  val rd = UInt(5.W)
}

class LabMaster extends Module {
  val io = IO(new Bundle {
    val busInterface = new BusInterface()
  })

  val pc = RegInit(0x80000000L.U)

  val idle :: fetch :: execution :: bus :: wb :: Nil = Enum(5)

  val state = RegInit(idle)
  val ins = RegInit(0.U(32.W))

  val registers = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))

  val busReq = Reg(new BusReq)

  val wbData = RegInit(0.U(32.W))
  val wbDst = RegInit(0.U(5.W))

  io.busInterface.cyc := false.B
  io.busInterface.stb := false.B
  io.busInterface.addr := 0.U
  io.busInterface.dataBytesSelect := 0.U
  io.busInterface.dataMode := false.B
  io.busInterface.dataWrite := 0.U

  when(state === idle) {
    state := fetch
  }.elsewhen(state === fetch) {
    // Fetch Stage
    io.busInterface.cyc := true.B
    io.busInterface.stb := true.B
    io.busInterface.addr := pc
    io.busInterface.dataBytesSelect := "b1111".U
    io.busInterface.dataMode := false.B
    io.busInterface.dataWrite := 0.U

    when(io.busInterface.ack) {
      state := execution
      ins := io.busInterface.dataRead
      io.busInterface.cyc := false.B
      io.busInterface.stb := false.B
    }

  }.elsewhen(state === execution) {
//     // Execution Stage
    val opcode = ins(6, 2)
    val func = ins(14, 12)
    val immTypeI = Cat(Fill(21, ins(31)), ins(30, 25), ins(24, 21), ins(20))
    val immTypeS = Cat(Fill(21, ins(31)), ins(30, 25), ins(11, 8), ins(7)) 
    val immTypeB =
      Cat(Fill(20, ins(31)), ins(7), ins(30, 25), ins(11, 8), 0.U)
    val immTypeU = Cat(ins(31), ins(30, 20), ins(19, 12), Fill(12, 0.U))
    val immTypeJ =
      Cat(ins(31), ins(19, 12), ins(20), ins(30, 25), ins(24, 21), 0.U)
    val rd = ins(11, 7)
    val src1 = registers(ins(19, 15))
    val src2 = registers(ins(24, 20))

    pc := pc + 4.U

    when(opcode === "b01101".U) {
      // lui
      registers(rd) := immTypeU

      state := fetch

    }.elsewhen(opcode === "b11000".U) {

      // beq
      state := fetch
      when(src1 === src2) {
        pc := pc + immTypeB
      }
    }.elsewhen(opcode === "b00000".U) {
      // lb

      val addr = src1 + immTypeI
      busReq.addr := addr
      busReq.dataBytesSelect := UIntToOH(addr(1, 0))
      busReq.dataMode := false.B
      busReq.rd := rd

      state := bus

    }.elsewhen(opcode === "b01000".U) {

      val addr = src1 + immTypeS
      busReq.addr := addr
      busReq.dataMode := true.B
      busReq.dataWrite := src2

      when(func === "b000".U) {
        // sb
        busReq.dataBytesSelect := UIntToOH(addr(1, 0))
      }.elsewhen(func === "b010".U) {
        // sw
        busReq.dataBytesSelect := "b1111".U
      }
      state := bus

    }.elsewhen(opcode === "b00100".U) {
      when(func === "b000".U) {
        // addi
        wbData := src1 + immTypeI
      }.elsewhen(func === "b111".U) {
        // andi
        wbData := src1 & immTypeI
      }
      state := wb
      wbDst := rd

    }.elsewhen(opcode === "b01100".U && func === "b000".U) {
      // add
      wbData := src1 + src2
      wbDst := rd
      state := wb
    }

  }.elsewhen(state === bus) {
    // Bus Stage
    io.busInterface.cyc := true.B
    io.busInterface.stb := true.B
    io.busInterface.addr := busReq.addr
    io.busInterface.dataBytesSelect := busReq.dataBytesSelect
    io.busInterface.dataMode := busReq.dataMode
    io.busInterface.dataWrite := busReq.dataWrite

    when(io.busInterface.ack) {
      when(busReq.dataMode === false.B) {
        // 一定为lb
        val data = io.busInterface.dataRead.asTypeOf(Vec(4, UInt(8.W)))(
          busReq.addr(1, 0)
        )

        registers(busReq.rd) := Cat(Fill(24, data(7)), data)
      }

      state := idle
    }

  }.elsewhen(state === wb) {
    registers(wbDst) := wbData
    state := fetch
  }
  registers(0) := 0.U

}
