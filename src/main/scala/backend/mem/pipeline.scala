package core

import chisel3._
import chisel3.util._

class MemoryPipeline(index: Int) extends Module with InstructionConstants {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MemoryInstruction))

    val robComplete = new RobCompleteRequest // 连接ROB
    val regRead = Vec(2, new RegisterReadRequest)
    val regWrite = new RegisterWriteRequest
    val newStore = new NewStoreRequest // 连接storeBuffer
    val findStore = new StoreFindRequest // 连接storeBuffer
    val bus = BusMasterInterface()

    val robHead = Input(UInt(BackendConfig.robIdxWidth))
    val robEmpty = Input(Bool())
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  // F0: 仲裁发射
  // F1: 读寄存器、数据旁路
  // F2: 数据旁路；访问内存、store buffer
  // F3: 写入物理寄存器，提交ROB

  val stall = WireInit(false.B)

  val f0_ins = RegInit(0.U.asTypeOf(new MemoryInstruction))
  val f0_valid = RegInit(false.B)

  val f0_done = !stall || !f0_valid

  io.in.ready := f0_done

  when(f0_done) {
    f0_ins := io.in.bits
    f0_valid := io.in.valid
    
    when (io.in.valid) {
      DebugUtils.Print(
        cf"[mem] Pipe${index} issue, robidx: ${io.in.bits.robIdx}, imm: ${io.in.bits.imm}, prs1: ${io.in.bits.prs1}, prd_or_prs2: ${io.in.bits.prd_or_prs2}, memType: ${io.in.bits.memType}"
      )
    }
  }

  // F1
  val f1_src1 = Reg(UInt(32.W))
  val f1_src2 = Reg(UInt(32.W))
  val f1_ins = RegInit(0.U.asTypeOf(new MemoryInstruction))
  val f1_valid = RegInit(false.B)

  val f1_sideway = BackendUtils.SearchSideway(f0_ins.prs1, f0_ins.prd_or_prs2)

  val f1_done = !stall || !f1_valid

  io.regRead(0).id := f0_ins.prs1
  io.regRead(1).id := f0_ins.prd_or_prs2

  when(f1_done) {
    f1_src1 := Mux(
      f1_sideway(0).valid,
      f1_sideway(0).value,
      io.regRead(0).value
    )
    f1_src2 := Mux(
      f1_sideway(1).valid,
      f1_sideway(1).value,
      io.regRead(1).value
    )
    f1_valid := f0_valid
    f1_ins := f0_ins

    when (!f0_done) {
      f0_valid := false.B
    }

  }

  // F2
  val f2_vaddr = Reg(UInt(BusConfig.ADDR_WIDTH))
  val f2_data = Reg(UInt(32.W))
  val f2_robIdx = Reg(UInt(BackendConfig.robIdxWidth))
  val f2_memType = Reg(Bool())
  val f2_memLen = Reg(UInt(MEM_LEN_WIDTH))
  val f2_bytes = Reg(UInt(4.W))
  val f2_prd = Reg(UInt(BackendConfig.pregIdxWidth))
  val f2_valid = RegInit(false.B)
  val f2_extType = RegInit(false.B)

  val f2_sideway = BackendUtils.SearchSideway(f1_ins.prs1, f1_ins.prd_or_prs2)
  val f2_src1_wire = Mux(f2_sideway(0).valid, f2_sideway(0).value, f1_src1)
  val f2_src2_wire = Mux(f2_sideway(1).valid, f2_sideway(1).value, f1_src2)

  val f2_vaddr_wire = f2_src1_wire + f1_ins.imm
  val f2_data_wire = f2_src2_wire

  val f2_done = !stall || !f2_valid

  // TODO: 使用f2_word_vaddr_wire访问 TLB，Dcache

  when(f2_done) {
    f2_vaddr := f2_vaddr_wire
    f2_data := f1_ins.getValue(f2_vaddr_wire, f2_data_wire)
    f2_robIdx := f1_ins.robIdx
    f2_memType := f1_ins.memType
    f2_bytes := f1_ins.getBytes(f2_vaddr_wire)
    f2_memLen := f1_ins.memLen

    f2_valid := f1_valid
    f2_prd := f1_ins.prd_or_prs2
    f2_extType := f1_ins.extType

    when (!f1_done) {
      f1_valid := false.B
    }
  } 

  when (!f1_done) {
    f1_src1 := f2_src1_wire
    f1_src2 := f2_src2_wire
  }


  // F3

  val f3_robIdx = Reg(UInt(BackendConfig.robIdxWidth))
  val f3_prd = Reg(UInt(BackendConfig.pregIdxWidth))
  val f3_writeRd = RegInit(false.B)

  val f3_data = Reg(UInt(32.W))
  val f3_valid = RegInit(false.B)
  val f3_storeType = RegInit(0.U(STORE_TYPE_WIDTH))
  val f3_storeBufferIdx = RegInit(0.U(BackendConfig.storeBufferIdxWidth))
  val f3_addrLow2 = RegInit(0.U(2.W))
  val f3_memLen = RegInit(0.U(2.W))
  val f3_extType = RegInit(false.B)

  val f3_bus_data = RegInit(false.B)

  // TODO : 从TLB接受地址
  val f3_word_paddr_wire = Cat(f2_vaddr(31, 2), 0.U(2.W))
  f3_addrLow2 := f2_vaddr(1, 0)

  DebugUtils.Print(cf"[mem] f3_word_paddr_wire 0x$f3_word_paddr_wire%x")

  io.findStore.paddr := f3_word_paddr_wire

  // 默认关闭
  io.newStore.valid := false.B
  io.newStore.store.bytes := f2_bytes
  io.newStore.store.value := f2_data
  io.newStore.store.paddr := f3_word_paddr_wire

  io.bus.master_turn_off()

  val f3_store_type_wire = WireInit(0.U(STORE_TYPE_WIDTH))
  val f3_wakeup_wire = WireInit(false.B)

  f3_robIdx := f2_robIdx
  f3_prd := f2_prd
  f3_writeRd := !f2_memType
  f3_storeType := f3_store_type_wire
  f3_memLen := f2_memLen
  f3_extType := f2_extType
  

  when(f2_valid) {
    io.bus.addr := f3_word_paddr_wire
    when(f2_memType) {
      // store
      // TODO : MMIO_BUFFER
      f3_store_type_wire := Mux(io.bus.mmio, STORE_MMIO, STORE_RAM)
      io.newStore.valid := true.B
      when(io.newStore.succ) {
        // 写入 Store Buffer 成功
        f3_valid := true.B
        f3_storeBufferIdx := io.newStore.idx
      }.otherwise {
        // 写入 Store Buffer 失败，需要停顿重试
        f3_valid := false.B
        stall := true.B
      }

    }.otherwise {
      // load
      // TODO : LOAD_MMIO
      f3_store_type_wire := Mux(io.bus.mmio, LOAD_MMIO, LOAD_RAM)
      when(io.findStore.valid && ~io.bus.mmio) {
        // 在 Store Buffer 中找到数据

        when((io.findStore.bytes & f2_bytes) === f2_bytes) {
          // 如果找到的修改是load的子集, 可以直接使用数据
          f3_valid := true.B
          // TODO: NOTE Half、Byte等需要在这里变换数据
          f3_data := io.findStore.value
          f3_bus_data := false.B
          f3_wakeup_wire := true.B
        }.otherwise {
          // 如果不是，需要停顿直到查不到或者属于子集为止
          stall := true.B
          f3_valid := false.B
        }

      }.otherwise {
        // 在 Buffer 中找不到数据，需要自行 load
        
        // TODO: 搜索Dcache结果
        // when (io.bus.stb) {
        //   DebugUtils.Print(cf" [mem] load, addr: 0x${f3_word_paddr_wire}%x, ack ${io.bus.ack}, data 0x${io.bus.dataRead}%x")
        // }

        // DebugUtils.Print(cf" ${io.robHead} === ${f2_robIdx} && !${io.robEmpty} && ${io.findStore.empty}")


        // 如果是MMIO，必须storeBuffer所有内容已经写回、并且确定可以提交后再开始load
        io.bus.stb := !io.bus.mmio || (io.robHead === f2_robIdx && !io.robEmpty && io.findStore.empty)
        io.bus.dataBytesSelect := "b1111".U
        io.bus.dataMode := false.B
        io.bus.dataWrite := DontCare

        when(io.bus.ack) {
          f3_bus_data := true.B
          f3_valid := true.B
          f3_wakeup_wire := true.B
        }.otherwise {
          stall := true.B
          f3_valid := false.B
        }
      }
    }
  } .otherwise {
    stall := false.B
    f3_valid := false.B
  }

  BackendUtils.BroadcastWakeup(index, f2_prd, f3_wakeup_wire)

  // F4 写回ROB

  val f4_write_rd_wire = WireInit(f3_writeRd && f3_valid)

  val f4_raw_data_wire = Mux(f3_bus_data, io.bus.dataRead, f3_data) >> (f3_addrLow2 << 3)
  val f4_data_wire = WireInit(
    MuxLookup(f3_memLen, 0.U)(
      Seq(
        MEM_BYTE -> Cat(Mux(f3_extType, Fill(24, f4_raw_data_wire(7)), 0.U(24.W)), f4_raw_data_wire(7, 0)),
        MEM_HALF -> Cat(Mux(f3_extType, Fill(16, f4_raw_data_wire(15)), 0.U(16.W)), f4_raw_data_wire(15, 0)),
        MEM_WORD -> f4_raw_data_wire
      )
    )
  )

  BackendUtils.BroadcastSideway(index, f3_prd, f4_data_wire, f4_write_rd_wire)

  io.regWrite.valid := f4_write_rd_wire
  io.regWrite.id := f3_prd
  io.regWrite.value := f4_data_wire

  io.robComplete.valid := f3_valid
  io.robComplete.robIdx := f3_robIdx
  io.robComplete.jump := false.B
  io.robComplete.exception := false.B
  io.robComplete.exceptionCode := DontCare
  io.robComplete.jumpTarget := DontCare
  io.robComplete.storeBufferIdx := f3_storeBufferIdx
  io.robComplete.storeType := f3_storeType

  if (DebugConfig.printWriteBack) {
    when(io.regWrite.valid) {
      DebugUtils.Print(
        cf"[mem] Pipe${index} writeback, rd: ${f3_prd}, value: ${f4_data_wire}"
      )
    }
    when(io.robComplete.valid) {
      DebugUtils.Print(
        cf"[mem] complete${index}, robidx: ${io.robComplete.robIdx}"
      )
    }
  }

  

  when(ctrlIO.flush) {
    // io.bus.stb := false.B
    f0_valid := false.B
    f1_valid := false.B
    f2_valid := false.B
    f3_valid := false.B
  }

}
