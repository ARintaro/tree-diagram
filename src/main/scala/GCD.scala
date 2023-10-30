// See README.md for license details.

package core

import chisel3._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1        = Input(UInt(16.W))
    val value2        = Input(UInt(16.W))
    val loadingValues = Input(Bool())
    val outputGCD     = Output(UInt(16.W))
    val outputValid   = Output(Bool())
  })

  val x  = Reg(UInt())
  val y  = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U
}

class Test extends Module {
  val io = IO(new Bundle {
    val x = Input(UInt(4.W))
    val reg = Output(UInt(4.W))
  }) 

  val count = RegInit(0.U(8.W))
  val reg = RegInit(0.U(4.W))

  count := count + 1.U

  when (count === 2.U) {
    reg := count
  }

  io.reg := reg
}