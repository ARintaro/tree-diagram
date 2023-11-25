package core

import chisel3._
import chisel3.util._


class MemoryPipeline extends Module {
  val io = IO(new Bundle {
	val in = Flipped(Decoupled(new MemoryInstruction))

	val robComplete = new RobCompleteRequest
	val regRead = new RegisterReadRequest
	val newStore = new NewStoreRequest
	val findStore = new StoreFindRequest
	val bus = BusMasterInterface()
  })

  val ctrlIO = IO(new Bundle {
	val flush = Input(Bool())
  })

  io.regRead.id := io.in.bits.prs

  // 访存流水线有三个流水段

  // F0: 仲裁发射/读寄存器/访问TLB、DCACHE
  // F1: 接收TLB、DCACHE数据，检验有效性，写入store buffer，进行唤醒
  // F2: 写入物理寄存器，提交ROB

  val f0_ins = RegInit(0.U.asTypeOf(new MemoryInstruction))
  val f0_vaddr = RegInit(0.U(32.W))
  val f0_valid = RegInit(false.B)
  val f0_done = WireInit(false.B)
  
  when (f0_done) {
	io.in.ready := true.B
	f0_ins := io.in.bits
	f0_vaddr := io.regRead.value + io.in.bits.imm
	f0_valid := io.in.valid
	// TODO : 在这里进行TLB与dcache访问

  } .otherwise {
	io.in.ready := false.B
  }

  when (f0_valid) {
	when (f0_ins.memType) {
	  // Store

	  // TODO : 根据总线返回确认是MMIO，如果是MMIO不应该写入store buffer，单独写一个MMIO Store
	  io.newStore.valid := true.B
	  // TODO : 补完getStoreIns()
	  io.newStore.store := f0_ins.getStoreIns()
	  
	  when (io.newStore.succ) {
		f0_done := true.B
		f1_robIdx := f0_ins.robIdx
		f1_bufferIdx := io.newStore.idx
		f1_wb := false.B
		f1_valid := true.B

	  } .otherwise {
		f0_done := false.B
		f1_valid := false.B
	  }

	} .otherwise {
	  // Load

	  io.findStore.paddr := f0_vaddr & "hFFFFFFFC".U
	  // TODO : 补完getBytes
	  val bytes = f0_ins.getBytes()
	  // 在store中找到数据，并且这次查找位是store中写入位的子集
	  when (io.findStore.valid && ((io.findStore.bytes & bytes) === bytes)) {
		// 在buffer中找到数据
		f0_done := true.B
		f1_valid := true.B
		f1_robIdx := f0_ins.robIdx
		f1_wb := true.B
		f1_wbVal := io.findStore.value

		// TODO : 找到数据后，广播唤醒

	  } .otherwise {
		// buffer中找不到数据，需要自行load
		// TODO : Dcache
		// TODO : 在store buffer中仅找到一个修改时，在这里可以直接合并两个修改，无需等到store buffer写回



		f0_done := false.B
		f1_valid := false.B
	  }	
	}

  } .otherwise {
	f0_done := true.B
	f1_valid := false.B
  }

  
  val f1_robIdx = RegInit(0.U(BackendConfig.robIdxWidth))
  val f1_wbVal = RegInit(0.U(32.W))
  val f1_wb = RegInit(false.B)
  val f1_valid = RegInit(false.B)
  // Buffer 中的Idx，有可能是Store Buffer，也有可能是MMIO Buffer
  val f1_bufferIdx = RegInit(0.U(BackendConfig.storeBufferIdxWidth))

  // TODO : 是否为MMIO类型，如果为MMIO类型，ROB提交时应该在MMIO BUFERR中提交

  // TODO : 旁路网路广播

  // TODO : 完成物理寄存器的写入

  // TODO : rob complete

}

