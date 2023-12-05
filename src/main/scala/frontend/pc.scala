package core

import chisel3._
import chisel3.util._

class ProgramCounter(inputNum: Int, startAddr: BigInt) extends Module {
  val io = IO(new Bundle {
    // 注意，这里越后面的优先级越高
    val reqs = Vec(inputNum, Flipped(Valid(UInt(BusConfig.ADDR_WIDTH))))

    val vaddr = Output(UInt(BusConfig.ADDR_WIDTH))
  })

  val addrReg = RegInit(startAddr.U)


  val arbiter = Module(new Arbiter(UInt(BusConfig.ADDR_WIDTH), inputNum))

  arbiter.io.out.ready := true.B
  // 这里把输入请求反转
  arbiter.io.in.zip(io.reqs.reverse).foreach {
    case (in, req) => {
      in.valid := req.valid
      in.bits := req.bits
    }
  }

  when(arbiter.io.out.valid) {
    addrReg := arbiter.io.out.bits
    io.vaddr := arbiter.io.out.bits
  }.otherwise {
    io.vaddr := addrReg
  }

}
