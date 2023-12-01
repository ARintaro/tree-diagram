package core

import chisel3._
import chisel3.util._

class CircularQueue[T <: Data](
    gen: T,
    size: Int,
    enqPortNum: Int,
    deqPortNum: Int,
    debug_name: String
) extends Module {

  val sizeWidth = log2Ceil(size).W

  val io = IO(new Bundle {
    val enq = Vec(enqPortNum, Flipped(Decoupled(gen)))
    val deq = Vec(deqPortNum, Decoupled(gen))

    val count = Output(UInt(sizeWidth))
  })

  val ctrlIO = IO(new Bundle {

    val flush = Input(Bool())
  })

  val head = RegInit(0.U(sizeWidth))
  val tail = RegInit(0.U(sizeWidth))

  val ram = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))

  when(!ctrlIO.flush) {
    // 补码，会自动warpAround
    val count = tail - head

    io.count := count

    val readyPrefixMask =
      MaskUtil.GetPrefixMask(size)(count)(enqPortNum - 1, 0)
    io.enq.zipWithIndex.foreach { case (enq, i) =>
      enq.ready := readyPrefixMask(i)
    }

    val queueValidMask = MaskUtil.GetValidMask(head, tail)

    // 确定出队元素
    for (i <- 0 until deqPortNum) {
      val idx = head + i.U
      io.deq(i).bits := ram(idx)
      io.deq(i).valid := queueValidMask(idx) & io.deq(i).ready
    }

    // 移动队头指针
    head := head + PopCount(io.deq.map(_.valid))

    val doEnq = WireInit(VecInit(Seq.fill(enqPortNum)(false.B)))

    for (i <- 0 until enqPortNum) {
      val idx = tail + i.U
      // 防止tail === head，队列被塞满
      io.enq(i).ready := !queueValidMask(idx) && (idx + 1.U =/= head)
      when(io.enq(i).valid && io.enq(i).ready) {
        ram(idx) := io.enq(i).bits
        doEnq(i) := true.B
      }
    }

    tail := tail + PopCount(doEnq)
  
  }.otherwise {
    head := 0.U
    tail := 0.U
    io.enq.foreach(enq => {
      enq.ready := false.B
      enq.valid := DontCare
      enq.bits := DontCare
    })
    io.deq.foreach(deq => {
      deq.valid := false.B
      deq.ready := DontCare
      deq.bits := 0.U.asTypeOf(gen)
    })
    io.count := 0.U
  }

  require(isPow2(size))

}
