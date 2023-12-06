package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import CsrConstants._

class TimerWithArbiter(inputNum: Int) extends Module {
    val io = IO(new Bundle {
        val masters = Vec(inputNum, new BusSlaveInterface)
    })
    
    val timer = Module(new Timer)
    val arbiter = Module(new BusArbiter(inputNum))
    
    io.masters <> arbiter.io.masters
    arbiter.io.device <> timer.io.bus
}

class Timer extends Module {
    val io = IO(new Bundle {
        val bus = new BusSlaveInterface
    })
    
    // create mtime & mtimecmp register
    val mtime = RegInit(0.U(64.W))
    val mtimecmp = RegInit(0.U(64.W))

    // core logic of time interrupt
    val timerInterrupt = WireInit(mtimecmp <= mtime)
    BoringUtils.addSource(timerInterrupt, "timerInterrupt")
    
    // The bus might send load or store instructions
    // Aligned Addr might be in a 8-byte range for mtime and mtimecmp
    val signal1 = io.bus.addr === CSR_MTIME_MEM_ADDR
    val signal2 = io.bus.addr === CSR_MTIME_MEM_ADDR + 4.U
    val signal3 = io.bus.addr === CSR_MTIMECMP_MEM_ADDR
    val signal4 = io.bus.addr === CSR_MTIMECMP_MEM_ADDR + 4.U
    
    io.bus.dataRead := RegNext(MuxCase(0.U, Seq(
        signal1 -> mtime(31, 0),
        signal2 -> mtime(63, 32),
        signal3 -> mtimecmp(31, 0),
        signal4 -> mtimecmp(63, 32)
    )))
    io.bus.mmio := true.B 
    mtime := mtime + 1.U

    // 输出只有 ack, dataRead
    when (io.bus.stb) {
        when (io.bus.dataMode) {
            when (signal1) {
                mtime := Cat(mtime(63, 32), io.bus.dataWrite)
            }.elsewhen (signal2) {
                mtime := Cat(io.bus.dataWrite, mtime(31, 0))
            }.elsewhen (signal3) {
                mtimecmp := Cat(mtimecmp(63, 32), io.bus.dataWrite)
            }.elsewhen (signal4) {
                mtimecmp := Cat(io.bus.dataWrite, mtimecmp(31, 0))
            }
        }
    }
    io.bus.ack := RegNext(io.bus.stb)
}