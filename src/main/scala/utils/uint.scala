package core

import chisel3._
import chisel3.util._

object  UIntUtils {
  def GetAllOne(width : Int) = {
	// 不使用-1进行类型转换，这样能保留宽度
	("b" + "1" * width).U(width.W)
  }
}