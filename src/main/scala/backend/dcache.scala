package core

import chisel3._
import chisel3.util._

class DcacheEntry extends Bundle {
  // (1, 0)
  val tag = UInt(BackendConfig.dcacheTagWidth) 
  val bytesEnable = UInt(BusConfig.DATA_BYTES_NUM)
  val data = UInt(BusConfig.DATA_WIDTH)

}

class DcacheWriteInterface extends Bundle {
  val valid = Bool()
  val paddr = UInt(BusConfig.ADDR_WIDTH)
  val data = UInt(BusConfig.DATA_WIDTH)
  val bytesEnable = UInt(BusConfig.DATA_BYTES_NUM)

  def get_entry() : DcacheEntry = {
    val entry = Wire(new DcacheEntry)
    entry.tag := paddr(BackendConfig.dcacheTagEnd, BackendConfig.dcacheTagBegin)
    entry.bytesEnable := bytesEnable
    entry.data := data
    entry
  }
}

class DataCache extends Module {
  val f1_io = IO(new Bundle {
    val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  })

  val f2_io = IO(new Bundle {
    val entry = Output(new DcacheEntry)
    // 流水线优先级永远高于store buffer，因此无需ack
    val write = Input(new DcacheWriteInterface)
  })

  val storeIO = IO(new Bundle {
    val write = Input(new DcacheWriteInterface)
    val ack = Output(Bool())
  })

  val entry = WireInit(0.U.asTypeOf(new DcacheEntry))
  val entrySize = 1 << log2Ceil(entry.getWidth)

  val mem = Module(new Bram("DcacheRam", entrySize, BackendConfig.dataCacheSize, entrySize, false))

  mem.io.readAddr := f1_io.vaddr(BackendConfig.dcacheIndexEnd, BackendConfig.dcacheIndexBegin)
  f2_io.entry := mem.io.readData.asTypeOf(new DcacheEntry)

  
  val write = Mux(f2_io.write.valid, f2_io.write, storeIO.write)

  mem.io.writeEnable := write.valid
  mem.io.writeAddr := write.paddr(BackendConfig.dcacheIndexEnd, BackendConfig.dcacheIndexBegin)
  mem.io.writeData := write.get_entry().asUInt
  
  storeIO.ack := !f2_io.write.valid && storeIO.write.valid
  mem.ctrlIO.clear := false.B
}
