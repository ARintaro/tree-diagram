package test

import chisel3._
import chisel3.util._
import core.ExternalSramInterface
import core.SramConfig
import chisel3.experimental._
import Math.pow

class VirtualSram extends Module {
  val io = IO(Flipped(new ExternalSramInterface))

  val mem = SyncReadMem(pow(2, 20).toInt, UInt(32.W))
  val init :: writing :: reading :: Nil = Enum(3)
  val bytes = RegInit(0.U(4.W))


  val extendedByteDisable = Cat(
    Fill(8, bytes(3)),
    Fill(8, bytes(2)),
    Fill(8, bytes(1)),
    Fill(8, bytes(0))
  )
  bytes := io.bytesDisable
  io.dataRead := mem.read(io.addr(19, 0))// | extendedByteDisable
  val state_reg = RegInit(init)
  switch(state_reg) {
    is(init) {

      when(!io.readDisable) {
        // assert(bytes === "b0000".U)
        state_reg := reading
      }
      when(!io.writeDisable) {
        state_reg := writing
      }
    }
    is(reading) {
      // assert(bytes === "b0000".U)
      state_reg := init
    }
    is(writing) {
      state_reg := init
      mem.write(
        io.addr(19, 0),
        (io.dataWrite & (~extendedByteDisable)) + (io.dataRead & extendedByteDisable)
      )
    }
  }
}
