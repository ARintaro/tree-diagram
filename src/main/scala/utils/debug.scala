package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

object DebugUtils {
  def Print(content: Printable) = {
    if (DebugConfig.debug) {
      val counter = Wire(UInt(32.W))
      BoringUtils.addSink(counter, "debugCounter")
      printf(cf"[$counter] $content\n")
    }
  }
}
