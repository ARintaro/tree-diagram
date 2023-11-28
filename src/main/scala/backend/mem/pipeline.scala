package core

import chisel3._
import chisel3.util._


class MemoryPipeline(index: Int) extends Module with InstructionConstants {
  val io = IO(new Bundle {
	val in = Flipped(Decoupled(new MemoryInstruction))

	val robComplete = new RobCompleteRequest // 连接ROB
	val regRead = Vec(2, new RegisterReadRequest)
	val regWrite = new RegisterWriteRequest
	val newStore = new NewStoreRequest  // 连接storeBuffer
	val findStore = new StoreFindRequest  // 连接storeBuffer
	val bus = BusMasterInterface()
  })

  val ctrlIO = IO(new Bundle {
	val flush = Input(Bool())
  })

  // initialize
  io.newStore.valid := false.B
  io.newStore.store := DontCare
  io.findStore.paddr := DontCare
  io.bus.master_turn_off()

  io.regRead(0).id := io.in.bits.prs1 // 对于读&写寄存器指令，从rs1中读取基地址
  io.regRead(1).id := io.in.bits.prd_or_prs2 // 对于写寄存器指令从rs2中读取值

  // 访存流水线有三个流水段

  // F0: 仲裁发射后读寄存器/访问TLB、DCACHE
  // F1: 接收TLB、DCACHE数据，检验有效性；写入store buffer（store），进行唤醒（load）
  // F2: 写入物理寄存器，提交ROB

  val f0_ins = RegInit(0.U.asTypeOf(new MemoryInstruction))
  val f0_valid = RegInit(false.B)
  val f0_done = WireInit(false.B)
  val f0_src1 = Reg(UInt(32.W)) // 从rs1中读取基地址
  val f0_src2 = Reg(UInt(32.W)) // 从rs2中读取值(仅对store指令有效)

  val f1_rd = RegInit(0.U(BackendConfig.pregIdxWidth))
  val f1_storeType = RegInit(0.U(STORE_TYPE_WIDTH))
  val f1_robIdx = RegInit(0.U(BackendConfig.robIdxWidth))
  val f1_wbVal = RegInit(0.U(32.W))
  val f1_wb = RegInit(false.B)
  val f1_valid = RegInit(false.B)
  // Buffer 中的Idx，有可能是Store Buffer，也有可能是MMIO Buffer
  val f1_bufferIdx = RegInit(0.U(BackendConfig.storeBufferIdxWidth))
  
  val sideway0 = BackendUtils.SearchSideway(io.in.bits.prs1, io.in.bits.prd_or_prs2)

  when (f0_done) {
	io.in.ready := true.B
	f0_ins := io.in.bits
	f0_src1 := Mux(sideway0(0).valid, sideway0(0).value, io.regRead(0).value)
	f0_src2 := Mux(sideway0(1).valid, sideway0(1).value, io.regRead(1).value)
	f0_valid := io.in.valid
	// TODO : 在这里进行TLB与dcache访问

  } .otherwise {
	io.in.ready := false.B
  }

  val wakeupValid = WireInit(false.B)
  // F1
  val sideway1 = BackendUtils.SearchSideway(f0_ins.prs1, f0_ins.prd_or_prs2)
  when (f0_valid) {
	val paddr = Mux(sideway1(0).valid, sideway1(0).value, f0_src1) + f0_ins.imm
	val data = Mux(sideway1(1).valid, sideway1(1).value, f0_src2)
	val storeType = WireInit(0.U(STORE_TYPE_WIDTH))
	when (f0_ins.memType) {
	  // Store
	  
	  // TODO : 根据总线返回确认是MMIO，如果是MMIO不应该写入store buffer，单独写一个MMIO Store
	  storeType := STORE_RAM // 这里暂时先不考虑MMIO
	  io.newStore.valid := true.B
	  // TODO : 补完getStoreIns()
	  io.newStore.store := f0_ins.getStoreIns(paddr, data)
		when (io.newStore.succ) {
			f0_done := true.B
			f1_robIdx := f0_ins.robIdx
			f1_bufferIdx := io.newStore.idx
			f1_wb := false.B
			f1_valid := true.B
			f1_storeType := storeType
		} .otherwise {
			f0_done := false.B
			f1_valid := false.B
		}
	}.otherwise {
	
	  // Load
	  val alignedPaddr = Cat(paddr(31, 2), 0.U(2.W))
	  io.findStore.paddr := alignedPaddr
	  // TODO : 补完getBytes
	  val bytes = f0_ins.getBytes(paddr(1, 0))
	  // 在store中找到数据
	  when (io.findStore.valid) {
		// 如果查找位是store中写入位的子集
	  	when ((io.findStore.bytes & bytes) === bytes) {
			// 在buffer中找到数据
			f0_done := true.B
			f1_valid := true.B
			f1_robIdx := f0_ins.robIdx
			f1_wb := true.B
			f1_wbVal := io.findStore.value
			f1_rd := f0_ins.prd_or_prs2
			// TODO : 找到数据后，广播唤醒
			wakeupValid := true.B
		} .otherwise {
			// 等待，直到查不到或查到属于子集为止
			f0_done := false.B
			f1_valid := false.B
		}
	  } .otherwise {
		// buffer中找不到数据，需要自行load
		// TODO : Dcache
		// TODO : 在store buffer中仅找到一个修改时，在这里可以直接合并两个修改，无需等到store buffer写回
		io.bus.stb := true.B
		io.bus.addr := alignedPaddr
		io.bus.dataBytesSelect := bytes
		io.bus.dataMode := false.B
		io.bus.dataWrite := DontCare
		when (io.bus.ack) {
			f0_done := true.B
			f1_valid := true.B
			f1_robIdx := f0_ins.robIdx
			f1_wb := true.B
			f1_wbVal := io.bus.dataRead
			f1_rd := f0_ins.prd_or_prs2
			wakeupValid := true.B
		} .otherwise {
			f0_done := false.B
			f1_valid := false.B
		}
	  }
	}
  } .otherwise {
	f0_done := true.B
	f1_valid := false.B
  }
  BackendUtils.BroadcastWakeup(index, f0_ins.prd_or_prs2, wakeupValid) // 广播唤醒


  // F2

  val writeValid = WireInit(f1_valid && f1_wb)

  // TODO : 是否为MMIO类型，如果为MMIO类型，ROB提交时应该在MMIO BUFERR中提交

  // TODO : 旁路网路广播
  BackendUtils.BroadcastSideway(index, f1_rd, f1_wbVal, writeValid)
  
  // TODO : 完成物理寄存器的写入
  io.regWrite.valid := writeValid
  io.regWrite.id := f1_rd
  io.regWrite.value := f1_wbVal

  // TODO : rob complete
  io.robComplete.valid := f1_valid
  io.robComplete.robIdx := f1_robIdx
  io.robComplete.jump := false.B
  io.robComplete.jumpTarget := DontCare
  io.robComplete.exception := false.B
  io.robComplete.exceptionCode := 0.U
  io.robComplete.storeBufferIdx := f1_bufferIdx
  io.robComplete.storeType := f1_storeType

  // TODO : flush
  when (ctrlIO.flush) {
	f0_valid := false.B
	f1_valid := false.B
	io.regWrite.valid := false.B
	io.robComplete.valid := false.B
	io.newStore.valid := false.B
	assert(!io.in.valid)
  }	
}

