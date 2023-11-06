package core

import chisel3._

class CtrlInterface extends Bundle {
  val flush = Bool()
	// 清空tlb，可能需要很多个周期
  val clear = Bool() 
}
