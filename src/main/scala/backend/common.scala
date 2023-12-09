package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait IssueInstruction {

  def checkReady(busy: UInt): Bool
}

class SidewayResult extends Bundle {
  val valid = Bool()
  val value = UInt(32.W)
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
    if (DebugConfig.printWakeup) {
      when (valid) {
        DebugUtils.Print(
          cf"[wakeup] Preg${preg} is ready, wakeup${index} is broadcasted"
        )
      }
    }
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
    BoringUtils.addSink(busy, "busy")
    return busy
  }


  def GetSidewayPregName(index : Int) : String = {
    return s"SidewayPreg${index}"
  }
  def GetSidewayValueName(index : Int) : String = {
    return s"SidewayValue${index}"
  }
  def GetSidewayValidName(index : Int) : String = {
    return s"SidewayValid${index}"
  }

  def BroadcastSideway(index : Int, preg : UInt, value : UInt, valid : Bool) = {
    BoringUtils.addSource(preg, GetSidewayPregName(index))
    BoringUtils.addSource(value, GetSidewayValueName(index))
    BoringUtils.addSource(valid, GetSidewayValidName(index))
  }

  def SearchSideway(rs1 : UInt, rs2 : UInt) : Vec[SidewayResult] = {
    val valid = Wire(Vec(BackendConfig.sidewayNum, Bool()))
    val preg = Wire(Vec(BackendConfig.sidewayNum, UInt(BackendConfig.pregIdxWidth)))
    val value = Wire(Vec(BackendConfig.sidewayNum, UInt(32.W)))

    val ret = Wire(Vec(2, new SidewayResult)) // valid, value

    for (i <- 0 until BackendConfig.sidewayNum) {
      BoringUtils.addSink(valid(i), GetSidewayValidName(i))
      BoringUtils.addSink(preg(i), GetSidewayPregName(i))
      BoringUtils.addSink(value(i), GetSidewayValueName(i))
    }

    for (i <- 0 until 2) {
      val id = if (i == 0) rs1 else rs2
      val eq = preg.zip(valid).map{case (x, y) => {
        x === id && y
      }}
      
      assert(PopCount(eq) <= 1.U)

      ret(i).valid := eq.reduce(_ || _)
      ret(i).value := Mux1H(eq, value)
    }
    ret
  }


}

