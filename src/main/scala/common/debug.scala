package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class DebugMod extends Module {
  val counter = RegInit(0.U(32.W))
  counter := counter + 1.U
  BoringUtils.addSource(counter, "debugCounter")
}