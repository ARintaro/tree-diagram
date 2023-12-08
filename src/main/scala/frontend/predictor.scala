package core

import chisel3._
import chisel3.util._

class BranchDirectionEntry extends Bundle {
  // 之前的分支历史
  val cur = UInt(FrontendConfig.branchHistoryWidth.W)
  // 两位饱和计数器，0，1代表不跳转，2，3代表跳转
  val counters = Vec(1 << FrontendConfig.branchHistoryWidth, UInt(2.W))
}

class BranchPredictCommit extends Bundle {
  val valid = Bool()
  val vaddr = UInt(BusConfig.ADDR_WIDTH)
  val jump = Bool()
}

class BranchPredictor extends Module {
  val f1_io = IO(new Bundle {
    val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))
  })

  val f2_io = IO(new Bundle {})

  val commitIO = IO(Input(new BranchPredictCommit))

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  // 饱和计数器
  // val branchBuffer = Module(new Bram("BranchBuffer", (new BranchDirectionEntry).asUInt.getWidth, FrontendConfig.branchHistoryBufferSize, (new BranchDirectionEntry).asUInt.getWidth, false))
  // val newBranchBuffer = Mem()

  // // f1阶段，读入饱和计数器


  // // 历史

  // // 3位当前历史，

  // // 处理提交
  // val commitBuffer = RegInit(0.U.asTypeof(new BranchPredictCommit))
  
  // when (commitIO.valid) {
  //   commitBuffer := commitIO
  // } .otherwise {
	// commitBuffer.valid := false.B
  // }

  // when (commitBuffer.valid) {

  // }




}
