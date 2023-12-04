package core

import chisel3._
import chisel3.util._

import CsrConstants._

class Timer extends Module {
    val io = IO(new Bundle {
        val bus = new BusSlaveInterface
        val timerInterrupt = Output(Bool())
    })
    
    // create mtime & mtimecmp register
    val mtime = RegInit(0.U(64.W))
    val mtimecmp = RegInit(0.U(64.W))

    // core logic of time interrupt
    io.timerInterrupt := (mtimecmp <= mtime)
    
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
                mtime(31, 0) := io.bus.dataWrite
            }.elsewhen (signal2) {
                mtime(63, 32) := io.bus.dataWrite
            }.elsewhen (signal3) {
                mtimecmp(31, 0) := io.bus.dataWrite
            }.elsewhen (signal4) {
                mtimecmp(63, 32) := io.bus.dataWrite
            }
        }
    }
    io.bus.ack := RegNext(io.bus.stb)
}