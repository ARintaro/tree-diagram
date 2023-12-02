package test

import chisel3._ 
import chisel3.util._
import core.ExternalSramInterface
import core.SramConfig
import chisel3.experimental._
import Math.pow

class virtualSram extends Module {
    val io = IO(Flipped(new ExternalSramInterface))

    val mem = SyncReadMem(pow(2, 20).toInt, UInt(32.W))
    val init::writing::reading::Nil = Enum(3)
    //val dataReadBuffer = RegInit(0.U(32.W))
    //val oldDataBuffer = RegInit(0.U(32.W))
    //io.dataRead := dataReadBuffer
    val extendedByteDisable = Cat(Fill(8, io.bytesDisable(3)), Fill(8, io.bytesDisable(2)), Fill(8, io.bytesDisable(1)), Fill(8, io.bytesDisable(0)))

    io.dataRead := mem.read(io.addr(19, 0))
    val state_reg = RegInit(init)
    switch(state_reg) {
        is(init) {
            when(!io.readDisable) {
                state_reg := reading
            }
            when(!io.writeDisable) {
                state_reg := writing
                //oldDataBuffer := mem.read((io.addr - "h80000000".U) >> 2.U)
            }
        }
        is(reading) {
            state_reg := init
        }
        is(writing) {
            state_reg := init
            mem.write(io.addr(19, 0), (io.dataWrite & (~extendedByteDisable)) + (io.dataRead & extendedByteDisable))
        }
    }
}