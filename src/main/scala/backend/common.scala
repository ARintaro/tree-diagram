package core


import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

object WakeUpUtils {
  def GetWakeupName(index : Int) : String = {
	return s"WakeUp${index}"
  }

  def BroadcastWakeup(index : Int, preg : UInt) = {
	  BoringUtils.addSource(preg, GetWakeupName(index))
  }

  def GetWakeup() : Vec[UInt] = {
	  val wakeUp = Wire(Vec(BackendConfig.wakeUpNum, UInt(BackendConfig.pregIdxWidth)))
    for (i <- 0 until BackendConfig.wakeUpNum) {
      BoringUtils.addSink(wakeUp(i), GetWakeupName(i)) 
    }
    return wakeUp
  }
}