package core

import chisel3._

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