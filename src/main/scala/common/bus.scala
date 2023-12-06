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

  // assert(!io.master.stb || (PopCount(slavesSelect) === 1.U), "Bus Select Error")
  if (DebugConfig.printBusError) {
    when (io.master.stb && PopCount(slavesSelect) =/= 1.U) {
      DebugUtils.Print(cf"Bus Select Error at 0x${addr}%x, NOTE : this may not a bug, but a normal situation because of speculative execution")
    }
  }

  val dataReg = RegInit(0.U(BusConfig.DATA_WIDTH))

  when (io.master.ack) {
    dataReg := Mux1H(slavesSelect, io.slaves.map(_.dataRead))
  }

  io.master.dataRead := dataReg
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

class BusArbiter(inputNum : Int) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(inputNum, BusSlaveInterface())
    val device = BusMasterInterface()
  })

  val busy = RegInit(false.B)

  val lastReq = Reg(new Bundle {
    val master = UInt(log2Ceil(inputNum).W)
    val addr = UInt(BusConfig.ADDR_WIDTH)
    val dataMode = Bool()
    val dataWrite = UInt(BusConfig.DATA_WIDTH)
    val dataBytesSelect = UInt(BusConfig.DATA_BYTES_NUM)
    val dataRead = UInt(BusConfig.DATA_WIDTH)
  })

  for (i <- 0 until inputNum) {
    io.masters(i).ack := false.B
    io.masters(i).dataRead := io.device.dataRead
    io.masters(i).mmio := io.device.mmio
  }

  when (busy) {
    io.device.stb := true.B
    io.device.addr := lastReq.addr
    io.device.dataBytesSelect := lastReq.dataBytesSelect
    io.device.dataMode := lastReq.dataMode
    io.device.dataWrite := lastReq.dataWrite

    when (io.device.ack) {
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

      io.device.stb := true.B
      io.device.addr := master.addr
      io.device.dataBytesSelect := master.dataBytesSelect
      io.device.dataMode := master.dataMode
      io.device.dataWrite := master.dataWrite

      busy := true.B
    } .otherwise {
      io.device.stb := false.B
      io.device.addr := DontCare
      io.device.dataBytesSelect := DontCare
      io.device.dataMode := DontCare
      io.device.dataWrite := DontCare      
    }

  }
  
}