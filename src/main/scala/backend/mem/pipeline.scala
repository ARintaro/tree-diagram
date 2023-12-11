package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import PrivilegeLevel._

class MemoryPipeline(index: Int) extends Module with InstructionConstants {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MemoryInstruction))

    val robComplete = new RobCompleteRequest // 连接ROB
    val regRead = Vec(2, new RegisterReadRequest)
    val regWrite = new RegisterWriteRequest
    val newStore = new NewStoreRequest // 连接storeBuffer
    val findStore = new StoreFindRequest // 连接storeBuffer
    
    val devBus = BusMasterInterface()
    val memBus = BusMasterInterface()

    val robHead = Input(UInt(BackendConfig.robIdxWidth))
    // val robEmpty = Input(Bool())

    val cacheWrite = Output(new DcacheWriteInterface)
    val cacheFindVaddr = Output(UInt(BusConfig.ADDR_WIDTH))
    val cacheResult = Input(new DcacheEntry)
  })

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())

    val clearTLB = Input(Bool())
  })

  val dmmu = Module(new DataMemoryManagementUnit)
  val tlb = Module(new TranslationLookasideBuffer)

  dmmu.busIO <> io.memBus
  dmmu.ctrlIO.flush := ctrlIO.flush

  tlb.ctrlIO.clear := ctrlIO.clearTLB

  val satp = WireInit(0.U.asTypeOf(new csr_satp_t))
  val priv = WireInit(0.U(2.W))
  BoringUtils.addSink(priv, "globalPrivilegeLevel")
  BoringUtils.addSink(satp, "satp")

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

    if (DebugConfig.printMem) {
      when(f0_valid) {
        DebugUtils.Print(
          cf"[mem] Pipe${index} Issue, robIdx: ${io.in.bits.robIdx}"
        )
      }
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

    when(!f0_done) {
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

  io.cacheFindVaddr := f2_vaddr_wire
  tlb.io.search := f2_vaddr_wire.asTypeOf(new VirtualAddress).GetTag(satp.asid)

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

    when(!f1_done) {
      f1_valid := false.B
    }
  }

  when(!f1_done) {
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
  val f3_vaddr = RegInit(0.U(32.W))
  val f3_exception = RegInit(0.U.asTypeOf(new Exception))

  val f3_first_in = RegInit(false.B)
  val f3_paddr_valid = RegInit(false.B)
  val f3_paddr = RegInit(0.U(BusConfig.ADDR_WIDTH))

  f3_first_in := !stall

  val f3_paddr_wire = WireInit(0.U(BusConfig.ADDR_WIDTH))
  val f3_paddr_valid_wire = WireInit(false.B)
  val f3_word_paddr_wire = Cat(f3_paddr_wire(31, 2), 0.U(2.W))

  f3_addrLow2 := f3_paddr_wire(1, 0)

  io.findStore.paddr := f3_word_paddr_wire

  // 默认关闭
  io.newStore.valid := false.B
  io.newStore.store.bytes := f2_bytes
  io.newStore.store.value := f2_data
  io.newStore.store.paddr := f3_word_paddr_wire

  io.devBus.master_turn_off()

  io.devBus.dataBytesSelect := "b1111".U
  io.devBus.dataMode := false.B
  io.devBus.dataWrite := DontCare

  val f3_store_type_wire = WireInit(0.U(STORE_TYPE_WIDTH))
  val f3_wakeup_wire = WireInit(false.B)

  f3_robIdx := f2_robIdx
  f3_prd := f2_prd
  f3_writeRd := !f2_memType
  f3_storeType := f3_store_type_wire
  f3_memLen := f2_memLen
  f3_extType := f2_extType
  f3_vaddr := f2_vaddr

  dmmu.io.stb := false.B
  dmmu.io.dataMode := DontCare
  dmmu.io.vaddr := DontCare

  tlb.io.insert.submit := false.B
  tlb.io.insert.entry := DontCare
  tlb.io.insert.tag := DontCare

  when(f2_valid) {
    when(f3_first_in) {
      when(satp.mode === 0.U || priv === M_LEVEL) {
        f3_paddr_wire := f2_vaddr
        f3_paddr_valid_wire := true.B
      }.elsewhen(tlb.io.result.hit && false.B) {
        // 暂时关闭tlb
        val pte = tlb.io.result.pte
        f3_paddr_wire := Cat(pte.ppn1, pte.ppn0, f2_vaddr(11, 0))(31, 0)
        f3_paddr_valid_wire := true.B
      }.otherwise {
        f3_paddr_wire := DontCare
        f3_paddr_valid_wire := false.B

        dmmu.io.stb := true.B
        dmmu.io.dataMode := f2_memType
        dmmu.io.vaddr := f2_vaddr.asTypeOf(new VirtualAddress)
      }

      f3_paddr_valid := f3_paddr_valid_wire
      f3_paddr := f3_paddr_wire
      f3_exception.valid := false.B

    }.otherwise {
      f3_paddr_valid_wire := f3_paddr_valid
      f3_paddr_wire := f3_paddr

      when(dmmu.io.ack && !f3_paddr_valid) {
        f3_paddr_valid := true.B
        val pte = dmmu.io.entry
        f3_paddr := Cat(pte.ppn1, pte.ppn0, f2_vaddr(11, 0))(31, 0)
        f3_exception := dmmu.io.exception

        when (pte.V && !f3_exception.valid) {
          tlb.io.insert.tag := f2_vaddr.asTypeOf(new VirtualAddress).GetTag(satp.asid)
          tlb.io.insert.entry := pte
          tlb.io.insert.submit := true.B
        }

      }

    }

    when(f3_paddr_valid_wire) {
      io.devBus.addr := f3_word_paddr_wire
      when(f2_memType) {
        // store
        // TODO : MMIO_BUFFER
        f3_store_type_wire := Mux(io.devBus.mmio, STORE_MMIO, STORE_RAM)
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
        f3_store_type_wire := Mux(io.devBus.mmio, LOAD_MMIO, LOAD_RAM)

        when(io.findStore.valid && ~io.devBus.mmio) {
          // 在 Store Buffer 中找到数据

          when((io.findStore.bytes & f2_bytes) === f2_bytes) {
            // 如果找到的修改是load的子集, 可以直接使用数据
            f3_valid := true.B
            // TODO: NOTE Half、Byte等需要在这里变换数据
            f3_data := io.findStore.value
            f3_bus_data := false.B
            f3_wakeup_wire := true.B
            if (DebugConfig.printMem) {
              DebugUtils.Print(
                cf"[Mem Pipe $index] robIdx $f2_robIdx, Store Buffer Hit, bytes: ${io.findStore.bytes}, paddr: 0x${f3_word_paddr_wire}%x, data: 0x${io.findStore.value}%x"
              )
            }
          }.otherwise {
            // 如果不是，需要停顿直到查不到或者属于子集为止
            stall := true.B
            f3_valid := false.B
          }

        }.elsewhen(
          (io.cacheResult.bytesEnable & f2_bytes) === f2_bytes && io.cacheResult.tag === f3_word_paddr_wire(
            BackendConfig.dcacheTagEnd,
            BackendConfig.dcacheTagBegin
          ) && f3_first_in
        ) {
          assert(!io.devBus.mmio)
          // Dcache Hit
          f3_valid := true.B
          f3_bus_data := false.B
          f3_wakeup_wire := true.B
          f3_data := io.cacheResult.data

        }.otherwise {
          // 在 Buffer 中找不到数据，需要自行 load
          io.devBus.stb := !io.devBus.mmio || (io.robHead === f2_robIdx && io.findStore.empty)

          when(io.devBus.ack) {
            f3_bus_data := true.B
            f3_valid := true.B
            f3_wakeup_wire := true.B

          }.otherwise {
            stall := true.B
            f3_valid := false.B
          }
        }
      }
    }.otherwise {
      stall := true.B
      f3_valid := false.B
    }
    
    when (!f3_first_in && f3_exception.valid) {
      stall := false.B
      f3_valid := true.B
    }
  }.otherwise {
    stall := false.B
    f3_valid := false.B
  }

  BackendUtils.BroadcastWakeup(index, f2_prd, f3_wakeup_wire)


  // F4 写回ROB

  val f4_write_rd_wire = WireInit(f3_writeRd && f3_valid && !f3_exception.valid)

  val f4_raw_data_wire = Mux(f3_bus_data, io.devBus.dataRead, f3_data)
  val f4_shift_data_wire = f4_raw_data_wire >> (f3_addrLow2 << 3)
  val f4_data_wire = WireInit(
    MuxLookup(f3_memLen, 0.U)(
      Seq(
        MEM_BYTE -> Cat(
          Mux(f3_extType, Fill(24, f4_shift_data_wire(7)), 0.U(24.W)),
          f4_shift_data_wire(7, 0)
        ),
        MEM_HALF -> Cat(
          Mux(f3_extType, Fill(16, f4_shift_data_wire(15)), 0.U(16.W)),
          f4_shift_data_wire(15, 0)
        ),
        MEM_WORD -> f4_shift_data_wire
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
  io.robComplete.exception := f3_exception.valid
  io.robComplete.exceptionCode := f3_exception.code
  io.robComplete.jumpTarget := f3_vaddr
  io.robComplete.storeType := f3_storeType
  io.robComplete.csrTag := false.B

  // LOAD出的数据写回缓存
  io.cacheWrite.valid := f3_storeType === LOAD_RAM && f4_write_rd_wire && f3_bus_data
  io.cacheWrite.paddr := Cat(f3_paddr(31, 2), 0.U(2.W))
  io.cacheWrite.bytesEnable := "b1111".U
  io.cacheWrite.data := f4_raw_data_wire

  if (DebugConfig.printWriteBack) {
    when(io.regWrite.valid) {
      DebugUtils.Print(
        cf"[mem] Pipe${index} robIdx $f3_robIdx writeback, rd: ${f3_prd}, value: 0x${f4_data_wire}%x, bus_data: 0x${io.devBus.dataRead}%x"
      )
    }
    when(io.robComplete.valid) {
      DebugUtils.Print(
        cf"[mem] complete${index}, robidx: ${io.robComplete.robIdx}"
      )
    }
  }

  when(ctrlIO.flush) {
    f0_valid := false.B
    f1_valid := false.B
    f2_valid := false.B
    f3_valid := false.B
  }

}
