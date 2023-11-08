package core

import chisel3._
import chisel3.util.PriorityEncoderOH

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
    assert(head =/= tail)

    val size = 1 << head.getWidth

    val allOne = (1.U << size) - 1.U

    val leftShifted = allOne << head
    // head = 1, tail = 2
    // 110       011
    return Mux(tail >= head,
      leftShifted & ~(allOne << tail),
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
}
