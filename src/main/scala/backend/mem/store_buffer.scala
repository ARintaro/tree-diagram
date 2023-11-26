package core

import chisel3._
import chisel3.util._
import os.write

class StoreIns extends Bundle with InstructionConstants {
  // 物理地址，按字选择，低两位必须为0
  val paddr = UInt(BusConfig.ADDR_WIDTH)
  // 写入值
  val value = UInt(BusConfig.DATA_WIDTH)
  // 字节使能
  val bytes = UInt(4.W)
}

class StoreBus extends Bundle {
  val stb = Input(Bool())
  val store = Input(new StoreIns)
  val ack = Output(Bool())
}

class NewStoreRequest extends Bundle {
  val valid = Output(Bool())
  val store = Output(new StoreIns)

  val succ = Input(Bool())
  val idx = Input(UInt(BackendConfig.storeBufferIdxWidth))
}

class CommitStoreRequest extends Bundle {
  val valid = Output(Bool())
  val idx = Input(UInt(BackendConfig.storeBufferIdxWidth))
}

class StoreFindRequest extends Bundle {
  val paddr = Output(UInt(BusConfig.ADDR_WIDTH))
  val valid = Input(Bool())
  val value = Input(UInt(BusConfig.DATA_WIDTH))
  val bytes = Input(UInt(4.W))
}

// mmio接到其他buffer里
class StoreBuffer(findPortNum: Int) extends Module {
  val io = IO(new Bundle {
    val news = Flipped(new NewStoreRequest)
    val commits =
      Vec(BackendConfig.maxCommitsNum, Flipped(new CommitStoreRequest))
    val finds = Vec(findPortNum, new StoreFindRequest)
  })

  val busIO = IO(Flipped(new StoreBus))

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val commited = RegInit(0.U(BackendConfig.storeBufferSize.W))
  val valid = RegInit(0.U(BackendConfig.storeBufferSize.W))

  val stores = Reg(Vec(BackendConfig.storeBufferSize, new StoreIns))

  val notCommited = valid & ~commited

  val begin = RegInit(0.U(BackendConfig.storeBufferIdxWidth))
  val end = RegInit(0.U(BackendConfig.storeBufferIdxWidth))

  // 处理新建请求

  // 尝试合并
  val newHit = VecInit(
    stores.map(x => x.paddr === io.news.store.paddr)
  ).asUInt
  val newFoldHit = newHit & notCommited
  assert(PopCount(newFoldHit) <= 1.U)

  val newFoldSucc = newFoldHit.orR
  val newFoldHitIdx = OHToUInt(newFoldHit)

  val alloc = WireInit(0.U(BackendConfig.storeBufferSize.W))

  when(io.news.valid) {
    assert(io.news.store.paddr(1, 0) === 0.U)

    when(newFoldSucc) {
      val oldBytes = stores(newFoldHitIdx).bytes
      val newBytes = io.news.store.bytes
      stores(newFoldHitIdx).bytes := oldBytes | newBytes

      val oldData = stores(newFoldHitIdx).value.asTypeOf(Vec(4, UInt(8.W)))
      val newData = io.news.store.value.asTypeOf(Vec(4, UInt(8.W)))
      val write = Wire(Vec(4, UInt(8.W)))
      for (i <- 0 until 4) {
        write(i) := Mux(newBytes(i), newData(i), oldData(i))
      }
      stores(newFoldHitIdx).value := write.asUInt

      io.news.succ := true.B
      io.news.idx := newFoldHitIdx

    }.otherwise {
      val inc = end + 1.U
      when(inc =/= begin) {
        // 队列未满
        end := inc
        stores(end) := io.news.store
        alloc := UIntToOH(end)
      }.otherwise {
        // 队列已满
        io.news.succ := false.B
        io.news.idx := DontCare
      }
    }
  }.otherwise {
    io.news.succ := false.B
    io.news.idx := DontCare
  }

  // 选择已经commit的store写入外部存储
  val busy = RegInit(false.B)
  val writeStore = Reg(new StoreIns)

  val free = WireInit(0.U(BackendConfig.storeBufferSize.W))

  // 默认关闭
  busIO.stb := false.B
  busIO.store := DontCare

  when(busy) {
    busIO.stb := true.B
    busIO.store := writeStore
    when(busIO.ack) {
      busy := false.B
    }
  }.otherwise {
    when(commited(begin)) {
      busy := true.B
      busIO.stb := true.B
      busIO.store := stores(begin)
      writeStore := stores(begin)
      free := UIntToOH(begin)
    }

  }

  valid := valid | alloc & ~free
  // 处理commit
  commited := commited & ~free | io.commits
    .map(x => Mux(x.valid, UIntToOH(x.idx), 0.U))
    .reduce(_ | _)

  val queue = Wire(Vec(BackendConfig.storeBufferSize, new StoreIns))
  val queueValid = Wire(Vec(BackendConfig.storeBufferSize, Bool()))
  for (i <- 0 until BackendConfig.storeBufferSize) {
    queue(i) := stores(begin + i.U)
    queueValid(i) := valid(begin + i.U)
  }

  val revQueue = VecInit(queue.reverse)
  val revQueueValid = VecInit(queueValid.reverse)

  // 处理find
  for (i <- 0 until findPortNum) {
    val find = io.finds(i)
    val findEq = VecInit(
      revQueue.map(x => x.paddr === find.paddr)
    ).asUInt & revQueueValid.asUInt
    val findHit = findEq.orR
    val findHitIdx = PriorityEncoder(findEq)

    find.valid := findHit
    find.value := revQueue(findHitIdx).value
    find.bytes := revQueue(findHitIdx).bytes
  }

  val recover = RegInit(false.B)
  when(ctrlIO.flush) {
    recover := true.B
  }
  when(recover) {
    assert(!io.news.valid)
    assert(!io.commits.map(_.valid).reduce(_ | _))

    val newValid = valid & commited
    valid := newValid
    end := begin + PopCount(newValid)

  }

}