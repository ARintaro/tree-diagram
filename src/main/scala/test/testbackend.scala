package test

import chisel3._ 
import chisel3.util._ 
import core._

class BackendTestTop extends Module {
    val topIO = IO(new Bundle {
        val instructions = Vec(FrontendConfig.decoderNum, Flipped(Decoupled(new RawInstruction)))
    })

    val backend = Module(new Backend)
    val decoder = Module(new DecodeUnit)

    val debug = Module(new DebugModule)

    backend.io.in <> decoder.io.out
    decoder.io.nextDone := backend.io.renameDone
    decoder.ctrlIO.flush := false.B
    topIO.instructions <> decoder.io.in
}