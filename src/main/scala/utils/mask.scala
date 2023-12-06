package core

import chisel3._
import chisel3.util._

object MaskUtil {
  def GetPrefixMask(width: Int)(prefixLength: UInt): UInt = {
    return Mux(
      prefixLength === 0.U,
      0.U(width.W),
      ((1.U(width.W) << prefixLength) - 1.U)(width - 1, 0)
    )
  }

  def IsPrefixMask(mask: Int): Boolean = {
    val invMask = ~mask
    return (mask & (mask + 1)) == 0
  }

  def GetValidMask(head : UInt, tail : UInt) : UInt = {
    val size = 1 << head.getWidth

    val allOne = -1.S(size.W).asUInt

    val leftShifted = (allOne << head)(size - 1, 0)
    return Mux(tail >= head,
      leftShifted & ~(allOne << tail)(size - 1, 0),
      leftShifted | (allOne >> (size.U - tail))
    )(size - 1, 0)
  }

  def SelectFirstN(n : Int)(in : UInt) = {
    val sels = Wire(Vec(n, UInt(in.getWidth.W)))
    var mask = in

    for (i <- 0 until n) {
      sels(i) := PriorityEncoderOH(mask)
      mask = mask & ~sels(i)
    }

    sels
  }

  def GetWordMask(bytes : UInt) : UInt = {
    return Cat(
      Fill(8, bytes(3)),
      Fill(8, bytes(2)),
      Fill(8, bytes(1)),
      Fill(8, bytes(0))
    )
  }
}
