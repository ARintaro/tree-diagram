package core

import chisel3._
import chisel3.util._

class EmptyBundle extends Bundle

class BusSlaveInterface extends Bundle {
  // Master 发送请求时为真
  val stb = Input(Bool())
  // Ack 为真后下次上升沿，可以修改Addr、BytesSelect、DataMode、DataWrite四项输入
  // 并且读结果有效
  val ack = Output(Bool())


  val addr = Input(UInt(BusConfig.ADDR_WIDTH))
  // 分字节的使能端
  val dataBytesSelect = Input(UInt(BusConfig.DATA_BYTES_NUM))
  // True for write, False for read
  val dataMode = Input(Bool())
  // 写入数据
  val dataWrite = Input(UInt(BusConfig.DATA_WIDTH))
  // 读出数据
  val dataRead = Output(UInt(BusConfig.DATA_WIDTH))

  val mmio = Output(Bool())

  def master_turn_off() = {
    stb := false.B
    addr := DontCare
    dataBytesSelect := DontCare
    dataMode := DontCare
    dataWrite := DontCare
  }
}

object BusSlaveInterface {
  def apply() = new BusSlaveInterface
}

object BusMasterInterface {
  def apply() = Flipped(new BusSlaveInterface)  
}

class MMIOAddrRangeInterface extends Bundle {
  val start = UInt(BusConfig.ADDR_WIDTH)
  val mask = UInt(BusConfig.ADDR_WIDTH)
}

class BusMux (slaveNum: Int) extends Module {
  val io = IO(new Bundle {
    val master = Flipped(BusMasterInterface())
    val slaves = Vec(slaveNum, Flipped(BusSlaveInterface()))
    val allocate = Input(Vec(slaveNum, new MMIOAddrRangeInterface)) // 地址分配
  })

  val addr = io.master.addr
  val slavesSelect = io.allocate.map(x => (addr & x.mask) === x.start)
                                .scan(false.B)((a, b) => b & !a).tail
  val masterCycle = io.master.stb

  assert(PopCount(slavesSelect) === 1.U)

  io.master.dataRead := Mux1H(slavesSelect, io.slaves.map(_.dataRead))
  io.master.ack := Mux1H(slavesSelect, io.slaves.map(_.ack))
  io.master.mmio := Mux1H(slavesSelect, io.slaves.map(_.mmio))

  // slave
  io.slaves.zip(slavesSelect).foreach{case (slave, select) => {
    slave.stb := masterCycle & select
    slave.addr := addr
    slave.dataBytesSelect := io.master.dataBytesSelect
    slave.dataMode := io.master.dataMode & select
    slave.dataWrite := io.master.dataWrite
  }}
}