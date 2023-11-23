package test

import chisel3._ 
import chisel3.util._ 
import core._

class IfutestTopVerilator extends Module {
    val InstrIO = IO(new Bundle {
        val fetch = Vec(FrontendConfig.decoderNum, Decoupled(new RawInstruction()))
    })
    val IfutestTop = Module(new IfutestTop)
    InstrIO.fetch <> IfutestTop.InstrIO.fetch
    val virtualSram = Module(new virtualSram)
    virtualSram.io <> IfutestTop.sram0
}

