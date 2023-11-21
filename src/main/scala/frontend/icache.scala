package core

import chisel3._
import chisel3.util.log2Ceil
import chisel3.util.isPow2
import chisel3.util.OHToUInt
import chisel3.util.Mux1H
import chisel3.util.UIntToOH
import chisel3.util.Cat
import chisel3.util.Valid
import dataclass.data

class IcacheConfig(_wayNum : Int, _cacheLineSize : Int, _cacheLineNum : Int) {
  // 组相连路数
  val wayNum = _wayNum

  // 单个 Cacheline 容纳指令的数量（需要为2的次幂）
  val cacheLineSize = _cacheLineSize

  // Cacheline 的数量（需要为2的次幂）
  val cacheLineNum = _cacheLineNum

  // 指令宽度(bit)
  val insWidth = InsConfig.INS_WIDTH.get

  // dataRam的参数，注意这些参数仅对一路
  // 最终实例化时需要乘以路数
  val dataRamInsNum = cacheLineSize * cacheLineNum
  val dataRamReadWidth = cacheLineSize * insWidth
  val dataRamWriteWidth = insWidth
  val dataRamWriteDepth = dataRamInsNum

  // tagRam的参数，注意这些参数仅对一路
  // 最终实例化时需要乘以路数
  val tagRamReadWidth = insWidth
  val tagRamWriteWidth = tagRamReadWidth

  // 地址中构成一个cacheline中地址的部分
  val addrCachelineIndexBegin = 2
  val addrCachelineIndexEnd = addrCachelineIndexBegin + log2Ceil(cacheLineSize) - 1

  // 地址中用于tag、data ram寻址的部分
  val addrIndexBegin = 2 + log2Ceil(cacheLineSize)
  val addrIndexEnd = addrIndexBegin + log2Ceil(cacheLineNum) - 1
  
  // 地址中构成tag的部分
  val addrTagBegin = addrIndexEnd + 1
  val addrTagEnd = BusConfig.ADDR_WIDTH.get - 1

  // tagRam返回数据中，构成有效位的部分
  val tagValidBegin = 0
  val tagValidEnd = cacheLineSize - 1

  // tagRam返回数据中，构成tag的部分
  val tagTagBegin = tagValidEnd + 1
  val tagTagEnd = tagTagBegin + addrTagEnd - addrTagBegin

  def GetTagRamWriteAddr(index : UInt, way : UInt) : UInt = {
    return (index << log2Ceil(wayNum)) + way
  }

  def GetDataRamWriteAddr(index : UInt, way : UInt, cachelineIndex : UInt) : UInt = {
    return (((index << log2Ceil(wayNum)) + way) << log2Ceil(cacheLineSize)) + cachelineIndex
  }

  def Print() = {
    println(s"addrTagBegin: $addrTagBegin")
    println(s"addrTagEnd: $addrTagEnd")
  }


  // 一个指令四字节，起始地址(1, 0)位一定为0
  // 一个cacheLine会存储 'cacheLineSize'  条指令
  // 这部分会在地址里占 w1 = log2ceil(cacheLineSize) 位
  // 同理 'cacheLineNum' 会占 w2 = log2ceil(cacheLineNum) 位
  // 所以 (r = 2 + w1 + (w2 - 1), 2 + w1)位作为指令cache寻址部分（r <= 11时，对于寻址部分，虚拟地址等于物理地址）
  // (r + 1, 31)位作为指令cache Tag部分，当然，之后可能会附加ASID作为tag

  // 整个作为tag的bram读宽度将会是 32 * wayNum
  // 每个cacheline用其中的32位
  // 其中低位用作valid位，高位用作真正的tag
  // 比如说一个cacheline中有四条指令，(0, 3)位分别对应cacheline中的四条指令的有效性

  // 在目前的方案中
  // 一块18k BRAM是 2KB，36k BRAM是4kb
  // 一块18k 用来存32位tag，可以存512个tag，也就是512个cacheLine
  // 一个cacheline存4条指令，一块18k的tag对应2048个指令，能带两块36k的dataRam
  // 所以用一块18k的存tag，读64位，写32位
  // 用两块36k的组成两路dataRam
  // (11, 4)作为index，(3, 2)用于cacheline内检索
  // (31, 12)作为tag

  // NOTE:
  // 别忘了tagram初始化置为0！！

  require(isPow2(cacheLineSize))
  require(isPow2(cacheLineNum))
  require(isPow2(wayNum))
  // VIPT的必要条件
  require(addrIndexEnd <= 11)
  require(tagTagEnd < tagRamWriteWidth)
}




class InstructionCache(config : IcacheConfig) extends Module {
  val f1_io = IO(new Bundle {
    // 请求的pc，虚拟地址
    val vaddr = Input(UInt(BusConfig.ADDR_WIDTH))

  })

  val f2_io = IO(new Bundle {
    
    // val paddr_valid = Input(Bool())
    
    // val paddr = Input(UInt(BusConfig.ADDR_WIDTH))

    // 物理地址有效性，如果为false，说明发生了TLB缺失，输出指令均为无效
    // 上周期请求pc对应的物理地址
    val paddr = Flipped(Valid(UInt(BusConfig.ADDR_WIDTH)))

    val ins = Output(Vec(config.cacheLineSize, UInt(InsConfig.INS_WIDTH)))
    // 指令有效
    val valid = Output(Vec(config.cacheLineSize, Bool()))
  })

  // flush 流水线冲刷请求，在这里实际上是让上个周期发往bram的请求失效
  // clear 清空指令缓存，实际上就是清空tagRam
  // 拉高后icache会进入清理状态，需要很多个周期
  val ctrlIO = IO(Input(new CtrlInterface))

  val sramIO = IO(BusMasterInterface())
  
  val tagRam = Module(new Bram("IcacheTagRam", config.tagRamWriteWidth, config.cacheLineNum * config.wayNum, config.tagRamReadWidth * config.wayNum, true))
  val dataRam = Module(new Bram("IcacheDataRam", config.dataRamWriteWidth, config.dataRamWriteDepth * config.wayNum, config.dataRamReadWidth * config.wayNum, false))

  // 默认关闭写请求
  tagRam.io.master_turn_off_write()
  dataRam.io.master_turn_off_write()

  tagRam.ctrlIO.clear := ctrlIO.clear
  dataRam.ctrlIO.clear := ctrlIO.clear
  
  {
    // 处理这周期新的请求
	  val index = f1_io.vaddr(config.addrIndexEnd, config.addrIndexBegin)

  	tagRam.io.readAddr := index
  	dataRam.io.readAddr := index
  }

  // 是否需要查询内存
  val missed = WireInit(false.B)
  // 查询的物理地址
  val sramAddr = Wire(UInt(BusConfig.ADDR_WIDTH))
  sramAddr := DontCare
  // 当sram查询返回时，写入tag的地址
  val writeTagAddr = Wire(UInt(tagRam.config.writeAddrWidth.W))
  writeTagAddr := DontCare
  // 当sram查询返回时，写入tag的数据
  val writeTagData = Wire(UInt(tagRam.config.writeWidth.W))
  writeTagData := DontCare
  // 当sram查询返回时，写入data的地址
  val writeDataAddr = Wire(UInt(dataRam.config.writeAddrWidth.W))
  writeDataAddr := DontCare
  

  {
    // 处理上个周期读出来的tag和data

    // 默认输出无效
    f2_io.ins.foreach(_ := DontCare)
    f2_io.valid.foreach(_ := false.B)

    // 随机替换
    val randomWay = RegInit(0.U(log2Ceil(config.wayNum).W))
    randomWay := randomWay + 1.U

    // 物理地址有效、且没有冲刷请求时执行检索
    when (f2_io.paddr.valid && !ctrlIO.flush && tagRam.ctrlIO.valid && dataRam.ctrlIO.valid) {
      
      val targetTag = f2_io.paddr.bits(config.addrTagEnd, config.addrTagBegin)
      val index = f2_io.paddr.bits(config.addrIndexEnd, config.addrIndexBegin)

      val rawTags = tagRam.io.readData.asTypeOf(Vec(config.wayNum, UInt(config.tagRamReadWidth.W)))
      // 一个tag中(0, cacheLineSize - 1)为数据有效位
      // (cacheLineSize, cacheLineSize + tagSize - 1)为tag位
      val tags = rawTags.map(_(config.tagTagEnd, config.tagTagBegin))
      val valids = rawTags.map(_(config.tagValidEnd, config.tagValidBegin))
      val datas = dataRam.io.readData.asTypeOf(Vec(config.wayNum, Vec(config.cacheLineSize, UInt(config.insWidth.W))))
      // 80000  0001
      //wire?
      val tagEqual = VecInit(tags.map(_ === targetTag))
      val anyHit = tagEqual.reduce(_ || _)
      val select = Mux(anyHit, tagEqual.asUInt, UIntToOH(randomWay))

      val way = OHToUInt(select)

      val cachelineIndex = f2_io.paddr.bits(config.addrCachelineIndexEnd, config.addrCachelineIndexBegin)

      // 如果命中，选中命中的way
      // 如果没命中，随机挑选一个way
      val hitTag = Mux1H(select, tags)
      val hitValid = Mux1H(select, valids)
      val hitData = Mux1H(select, datas)
      
      // 0 1 2 3
      // 0 0 0 1
      // 32 32 32 32
      // 0 32 64 96
      // 2^5
      when (anyHit && hitValid(cachelineIndex)) {
        // 缓存命中
        f2_io.valid := (hitValid >> cachelineIndex).asTypeOf(Vec(config.cacheLineSize, Bool()))
        f2_io.ins := (hitData.asUInt >> (cachelineIndex << log2Ceil(config.dataRamWriteWidth))).asTypeOf(Vec(config.cacheLineSize, UInt(config.insWidth.W)))
      } .otherwise {
        // 缓存未命中
        missed := true.B
        sramAddr := f2_io.paddr.bits

        writeTagAddr := config.GetTagRamWriteAddr(index, way)

        val newValid = Mux(anyHit, hitValid | (1.U(config.cacheLineSize.W) << cachelineIndex), 1.U(config.cacheLineSize.W) << cachelineIndex)(config.cacheLineSize - 1, 0)
        writeTagData := Cat(targetTag, newValid)

        writeDataAddr := config.GetDataRamWriteAddr(index, way, cachelineIndex)
      }


    }
  }


  {
    // 默认关闭sram
    sramIO.master_turn_off()
    

    // 处理sram请求
    val busy = RegInit(false.B)
    val acked = RegInit(false.B)

    // 查询的物理地址
    val sramAddrReg = Reg(UInt(BusConfig.ADDR_WIDTH))
    // 当sram查询返回时，写入tag的地址
    val writeTagAddrReg = Reg(UInt(tagRam.config.writeAddrWidth.W))
    // 当sram查询返回时，写入tag的数据
    val writeTagDataReg = Reg(UInt(tagRam.config.writeWidth.W))
    // 当sram查询返回时，写入data的地址
    val writeDataAddrReg = Reg(UInt(dataRam.config.writeAddrWidth.W))


    when (busy) {
      sramIO.stb := true.B
      sramIO.addr := sramAddrReg
      sramIO.dataBytesSelect := "b1111".U
      sramIO.dataMode := false.B

      when (sramIO.ack) {
        busy := false.B
        acked := true.B
      }
    } .otherwise {
      when (acked) {
        tagRam.io.writeAddr := writeTagAddrReg
        tagRam.io.writeData := writeTagDataReg
        tagRam.io.writeEnable := true.B

        dataRam.io.writeAddr := writeDataAddrReg
        dataRam.io.writeData := sramIO.dataRead
        dataRam.io.writeEnable := true.B

        acked := false.B
        missed := false.B
      }

      when (missed) {
        sramAddrReg := sramAddr
        writeTagAddrReg := writeTagAddr
        writeTagDataReg := writeTagData
        writeDataAddrReg := writeDataAddr

        busy := true.B

        sramIO.stb := true.B
        sramIO.addr := sramAddr
        sramIO.dataBytesSelect := "b1111".U
        sramIO.dataMode := false.B
      }
    }
  }  
  
}
