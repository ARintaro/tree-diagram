package core

import chisel3._
import chisel3.util._
import os.write
import chisel3.util.experimental.BoringUtils

class StoreIns extends Bundle with InstructionConstants {
  // 物理地址，按字选择，低两位必须为0
  val paddr = UInt(BusConfig.ADDR_WIDTH)
  // 写入值
  val value = UInt(BusConfig.DATA_WIDTH)
  // 字节使能
  val bytes = UInt(4.W)
}

// class StoreBus extends Bundle {
//   val stb = Input(Bool())
//   val store = Input(new StoreIns)
//   val ack = Output(Bool())
// }

class NewStoreRequest extends Bundle {
  val valid = Output(Bool())
  val store = Output(new StoreIns)

  val succ = Input(Bool())
  val idx = Input(UInt(BackendConfig.storeBufferIdxWidth))
}

class CommitStoreRequest extends Bundle {
  val valid = Output(Bool())
  // val idx = Output(UInt(BackendConfig.storeBufferIdxWidth))
}

class StoreFindRequest extends Bundle {
  val paddr = Output(UInt(BusConfig.ADDR_WIDTH))
  val valid = Input(Bool())
  val value = Input(UInt(BusConfig.DATA_WIDTH))
  val bytes = Input(UInt(4.W))

  val empty = Input(Bool())
}


// class StoreBuffer(findPortNum: Int) extends Module {
//   val io = IO(new Bundle {
//     val news = Flipped(new NewStoreRequest)
//     val commits =
//       Vec(BackendConfig.maxCommitsNum, Flipped(new CommitStoreRequest))
//     val finds = Vec(findPortNum, Flipped(new StoreFindRequest))
//   })

//   val busIO = IO(BusMasterInterface())

//   val ctrlIO = IO(new Bundle {
//     val flush = Input(Bool())
//   })

//   busIO.master_turn_off()

//   val commited = RegInit(0.U(BackendConfig.storeBufferSize.W))
//   val valid = RegInit(0.U(BackendConfig.storeBufferSize.W))

//   val stores = Reg(Vec(BackendConfig.storeBufferSize, new StoreIns))

//   val notCommited = valid & ~commited

//   val begin = RegInit(0.U(BackendConfig.storeBufferIdxWidth))
//   val end = RegInit(0.U(BackendConfig.storeBufferIdxWidth))

//   // 处理新建请求

//   val alloc = WireInit(0.U(BackendConfig.storeBufferSize.W))

//   when(io.news.valid) {
//     assert(io.news.store.paddr(1, 0) === 0.U)

//     // Print io.news
//     if (DebugConfig.printStoreBuffer) {
//       DebugUtils.Print(
//         cf"store buffer new, idx: ${io.news.idx} paddr: 0x${io.news.store.paddr}%x, value: ${io.news.store.value}, bytes: ${io.news.store.bytes} "
//       )
//     }

//     val inc = end + 1.U
//     when(inc =/= begin) {
//       // 队列未满
//       end := inc
//       stores(end) := io.news.store
//       alloc := UIntToOH(end)
//       io.news.succ := true.B
//       io.news.idx := end
//     }.otherwise {
//       // 队列已满
//       io.news.succ := false.B
//       io.news.idx := DontCare
//     }
//   }.otherwise {
//     io.news.succ := false.B
//     io.news.idx := DontCare
//   }

//   // 选择已经commit的store写入外部存储
//   val busy = RegInit(false.B)
//   val writeStore = Reg(new StoreIns)

//   val free = WireInit(0.U(BackendConfig.storeBufferSize.W))

//   // 默认关闭
//   busIO.stb := false.B
//   // busIO.store := DontCare

//   when(busy) {
//     busIO.stb := true.B
//     // busIO.store := writeStore
//     busIO.dataMode := true.B
//     busIO.dataWrite := writeStore.value
//     busIO.dataBytesSelect := writeStore.bytes
//     busIO.addr := writeStore.paddr
//     when(busIO.ack) {
//       busy := false.B
//       if (DebugConfig.printStoreBuffer) {
//         DebugUtils.Print(
//           cf"store buffer bus ack, paddr: 0x${writeStore.paddr}%x, value: ${writeStore.value}, bytes: ${writeStore.bytes}"
//         )
//       }
//       free := UIntToOH(begin)
//       begin := begin + 1.U
//     }
//   }.otherwise {
//     when(commited(begin)) {
//       busy := true.B
//       busIO.stb := true.B
//       val curStore = stores(begin)
//       writeStore := curStore
//       busIO.dataMode := true.B
//       busIO.dataWrite := curStore.value
//       busIO.dataBytesSelect := curStore.bytes
//       busIO.addr := curStore.paddr
//     }

//   }

//   valid := (valid & ~free) | alloc
//   // 处理commit
//   commited := (commited & ~free) | io.commits
//     .map(x => Mux(x.valid, UIntToOH(x.idx), 0.U))
//     .reduce(_ | _)

//   // 循环打印io.commits
//   if (DebugConfig.printStoreBuffer) {
//     for (i <- 0 until BackendConfig.maxCommitsNum) {
//       when(io.commits(i).valid) {
//         DebugUtils.Print(
//           cf"store buffer commit, idx: ${io.commits(i).idx}, paddr: 0x${stores(
//               io.commits(i).idx
//             ).paddr}%x, value: ${stores(io.commits(i).idx).value}, bytes: ${stores(io.commits(i).idx).bytes}"
//         )
//       }
//     }

//     DebugUtils.Print("==== Store Buffer BEGIN==== ")

//     for (i <- 0 until BackendConfig.storeBufferSize) {
//       when(valid(i)) {
//         DebugUtils.Print(
//           cf"idx: ${i}, commit: ${commited(i)} paddr: 0x${stores(i).paddr}%x, value: ${stores(
//               i
//             ).value}, bytes: ${Binary(stores(i).bytes)}"
//         )
//       }
//     }
//     DebugUtils.Print("==== Store Buffer END ==== ")
//   }

//   val queue = Wire(Vec(BackendConfig.storeBufferSize, new StoreIns))
//   val queueValid = Wire(Vec(BackendConfig.storeBufferSize, Bool()))
//   for (i <- 0 until BackendConfig.storeBufferSize) {
//     queue(i) := stores(begin + i.U)
//     queueValid(i) := valid(begin + i.U)
//   }

//   val revQueue = VecInit(queue.reverse)
//   val revQueueValid = VecInit(queueValid.reverse)

//   // 处理find
//   for (i <- 0 until findPortNum) {
//     val find = io.finds(i)
//     val findEq = VecInit(
//       revQueue.map(x => x.paddr === find.paddr)
//     ).asUInt & revQueueValid.asUInt
//     val findHit = findEq.orR
//     val findHitIdx = PriorityEncoder(findEq)

//     find.valid := findHit
//     find.value := revQueue(findHitIdx).value
//     find.bytes := revQueue(findHitIdx).bytes
//   }

//   val recover = RegInit(false.B)
//   when(ctrlIO.flush) {
//     recover := true.B
//   }
//   when(recover) {
//     assert(!io.news.valid)
//     assert(!io.commits.map(_.valid).reduce(_ | _))

//     val newValid = valid & commited
//     valid := newValid
//     end := begin + PopCount(newValid)
//     recover := false.B
//   }

// }


class CompressedStoreBuffer(findPortNum : Int) extends Module {
  val io = IO(new Bundle {
    val news = Flipped(new NewStoreRequest)
    val commits = Vec(BackendConfig.maxCommitsNum, Flipped(new CommitStoreRequest))
    val finds = Vec(findPortNum, Flipped(new StoreFindRequest))

    val cache = new Bundle {
      val write = Output(new DcacheWriteInterface)
      val ack = Input(Bool())
    }
  })

  val busIO = IO(BusMasterInterface())

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val stores = Reg(Vec(BackendConfig.storeBufferSize, new StoreIns))
  val valid = RegInit(0.U(BackendConfig.storeBufferSize.W))
  val commited = RegInit(0.U(BackendConfig.storeBufferSize.W))

  val firstEmptyIdx = PriorityEncoder(~valid)

  // 首先处理commit，经过重排序缓存，commit一定按顺序提交，此时不需要idx
  val commitCount = PopCount(io.commits.map(_.valid))

  val newCommited = (commited << commitCount) | MaskUtil.GetPrefixMask(BackendConfig.storeBufferSize)(commitCount)


  busIO.master_turn_off()

  io.news.succ := !valid(BackendConfig.storeBufferSize - 1)
  io.news.idx := DontCare

  io.cache.write.valid := false.B
  io.cache.write.paddr := DontCare
  io.cache.write.data := DontCare
  io.cache.write.bytesEnable := DontCare

  val deq = WireInit(false.B)
  val enq = io.news.valid && io.news.succ
  
  val busBusy = RegInit(false.B)
  val sramAcked = RegInit(false.B)
  val cacheAcked = RegInit(false.B)
  when (busBusy) {
    val sramAck = sramAcked | busIO.ack
    val cacheAck = cacheAcked | io.cache.ack

    busIO.stb := !sramAcked
    busIO.dataMode := true.B
    busIO.dataWrite := stores(0).value
    busIO.dataBytesSelect := stores(0).bytes
    busIO.addr := stores(0).paddr

    io.cache.write.valid := !cacheAcked
    io.cache.write.paddr := stores(0).paddr
    io.cache.write.data := stores(0).value
    io.cache.write.bytesEnable := stores(0).bytes

    sramAcked := sramAck
    cacheAcked := cacheAck

    // DebugUtils.Print(cf"store buffer, sramAck ${sramAck} cache ${cacheAck}")

    when (sramAcked && cacheAck) {
      busBusy := false.B
      deq := true.B
    }
  } .otherwise {
    when (commited(0)) {
      busBusy := true.B
      busIO.stb := true.B
      busIO.dataMode := true.B
      busIO.addr := stores(0).paddr
      busIO.dataWrite := stores(0).value
      busIO.dataBytesSelect := stores(0).bytes
      
      cacheAcked := busIO.mmio
      sramAcked := false.B      
    }
  }


  when (deq) {
    for (i <- 0 until BackendConfig.storeBufferSize - 1) {
      stores(i) := stores(i + 1)
    }
    commited := newCommited >> 1
  } .otherwise {
    commited := newCommited
  }

  when (enq) {
    val enqIdx = Mux(deq, firstEmptyIdx - 1.U, firstEmptyIdx)
    stores(enqIdx) := io.news.store

    if (DebugConfig.printStoreBuffer) {
      // printf(
      //   cf"store buffer enq, paddr: 0x${io.news.store.paddr}%x, 0xvalue: ${io.news.store.value}%x, bytes: ${io.news.store.bytes}\n"
      // )
    }
  }

  when(!enq && deq) {
    valid := valid >> 1
  } .elsewhen(enq && !deq) {
    valid := (valid << 1) | 1.U
  }


  // 处理find
  for (i <- 0 until findPortNum) {
    val find = io.finds(i)
    val findEq = VecInit(
      stores.map(x => x.paddr === find.paddr)
    ).asUInt & valid.asUInt
    val findHit = findEq.orR
    val findHitIdx = PriorityEncoder(findEq.asBools.reverse)

    val reverseStore = VecInit(stores.reverse)

    find.valid := findHit
    find.value := reverseStore(findHitIdx).value
    find.bytes := reverseStore(findHitIdx).bytes
    find.empty := !valid(0)
  }

  if (DebugConfig.printStoreBuffer) {
    DebugUtils.Print("==== Store Buffer BEGIN==== ")

    for (i <- 0 until BackendConfig.storeBufferSize) {
      when(valid(i)) {
        DebugUtils.Print(
          cf"idx: ${i}, commit: ${commited(i)} paddr: 0x${stores(i).paddr}%x, value: 0x${stores(
              i
            ).value}%x, bytes: ${Binary(stores(i).bytes)}"
        )
      }
    }
    DebugUtils.Print("==== Store Buffer END ==== ")

  }

  when (ctrlIO.flush) {
    // 由于flush信号延时一周期，可以保证这周期没有commit、并无视这周期的入队请求
    assert(!io.commits.map(_.valid).reduce(_ | _))
    
    when (deq) {
      valid := commited >> 1
    } .otherwise {
      valid := commited
    }
  }
}