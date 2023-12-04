package core

import chisel3._
import chisel3.util._

class TimeUnit extends Module {
    val io = IO(new Bundle {
        val busIO = Flipped(new BusSlaveInterface)
        val timerInterrupt = Output(Bool())
    })
    
    // create mtime & mtimecmp register
    val mtime = RegInit(0.U(64.W))
    val mtimecmp = RegInit(0.U(64.W))

    // core logic of time interrupt
    io.timerInterrupt := (mtimecmp < mtime)
    
    // The bus might send load or store instructions
    // Aligned Addr might be in a 8-byte range for mtime and mtimecmp

}