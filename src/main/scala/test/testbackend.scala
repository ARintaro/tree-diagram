package test

import chisel3._ 
import chisel3.util._ 
import core.Backend
import core.DecodeUnit
import core.FrontendConfig
import core.RawInstruction

class BackendTestTop extends Module {
    val topIO = IO(new Bundle {
        val instructions = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    })

    val backend = Module(new Backend)
    val decoder = Module(new DecodeUnit)

    backend.io.in <> decoder.io.out
    decoder.io.nextDone := backend.io.renameDone
    decoder.ctrlIO.flush := false.B
    topIO.instructions <> decoder.io.in
    
}