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
    val write = Input(new DcacheWriteInterface)
  })

  val storeIO = IO(Input(new DcacheWriteInterface))

  val mem = SyncReadMem(BackendConfig.dataCacheSize, new DcacheEntry, SyncReadMem.ReadFirst)

  f2_io.entry := mem.read(f1_io.vaddr(BackendConfig.dcacheIndexEnd, BackendConfig.dcacheIndexBegin))

  // when (storeIO.valid) {
  //   mem.write(storeIO.paddr, storeIO.data)
  // }

  

}
