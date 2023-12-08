package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

object DebugUtils {
  def Print(content: Printable) = {
    if (DebugConfig.debug) {
      val counter = Wire(UInt(32.W))
      BoringUtils.addSink(counter, "debugCounter")
      if (DebugConfig.printPart) {
        when (DebugConfig.printBegin <= counter && counter <= DebugConfig.printEnd){
          printf(cf"[$counter] $content\n")
        }
      }
      else {
        printf(cf"[$counter] $content\n")
      }
    }
  }
}
