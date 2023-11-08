package core

import chisel3._
import chisel3.util.log2Ceil

object GenConfig {
  var verilator = false
}

object DebugConfig {
  val debug = true
  val print_state = true
}

object SramConfig {
  val ADDR_WIDTH = 20.W
  val DATA_WIDTH = 32.W
  val DATA_BYTES_NUM = (DATA_WIDTH.get / 8).W
}

object BusConfig {
  val DATA_WIDTH = 32.W
  val DATA_BYTES_NUM = (DATA_WIDTH.get / 8).W
  val ADDR_WIDTH = 32.W
}

object InsConfig {
  val INS_WIDTH = 32.W

  object Opcode {
    val jal = "b1101111".U
    val jalr = "b1100111".U
    val branch = "b1100011".U
  }
}

object CacheConfig {
  val icache = new IcacheConfig(2, 4, 256)
}

object FrontendConfig {
  val fetchQueueSize = 16

  val decoderNum = 3
}

object BackendConfig {
  val physicalRegNum = 64
  val robSize = 32

  val pregIdxWidth = log2Ceil(physicalRegNum).W
  val robIdxWidth = log2Ceil(robSize).W

  val maxCommitsNum = 3 
}