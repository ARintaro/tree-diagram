package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait IssueInstruction {

  def checkReady(busy: UInt): Bool
}

object BackendUtils {
  def GetWakeupName(index: Int): String = {
    return s"WakeUp${index}"
  }
  def GetWakeupValidName(index: Int): String = {
    return s"WakeUp${index}-Valid"
  }

  // 广播后的下一个周期依赖指令才能够发射
  def BroadcastWakeup(index: Int, preg: UInt, valid : Bool) = {
    BoringUtils.addSource(preg, GetWakeupName(index))
    BoringUtils.addSource(valid, GetWakeupValidName(index))
  }

  def GetWakeupHot(): UInt = {
    val wakeup = Wire(
      Vec(BackendConfig.wakeUpNum, UInt(BackendConfig.pregIdxWidth))
    )
    val valid = Wire(
      Vec(BackendConfig.wakeUpNum, Bool())
    )
    for (i <- 0 until BackendConfig.wakeUpNum) {
      BoringUtils.addSink(wakeup(i), GetWakeupName(i))
      BoringUtils.addSink(valid(i), GetWakeupValidName(i))
    }

    val onehot = wakeup.zip(valid).map{case (x, y) => {
      Mux(y, UIntToOH(x), 0.U)
    }}.reduce(_ | _)

    return onehot
  }

  def GetBusy() : UInt = {
    val busy = Wire(UInt(BackendConfig.physicalRegNum.W))
    return busy
  }
}
