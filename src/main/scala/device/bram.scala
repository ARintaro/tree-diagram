package core

import chisel3._
import chisel3.util._
import chisel3.experimental._
import scala.collection.mutable

abstract class BramConfig {
  val writeWidth : Int
  val writeDepth : Int
  val readWidth : Int
  val totalSpace : Int
  val readDepth : Int

  val readNum = readWidth / writeWidth

  val writeAddrWidth = log2Ceil(writeDepth)
  val readAddrWidth = log2Ceil(readDepth)

  def printMembers(): Unit = {
    println(s"type : Simple Dual Port Ram")
    println(s"writeWidth : $writeWidth")
    println(s"writeDepth : $writeDepth")
    println(s"Write First Always Enabled")
    println(s"No Output Register")

    println(s"readWidth : $readWidth")
    println(s"totalSpace : $totalSpace")
    println(s"readDepth : $readDepth")

    println(s"Init With 0")
  }

  require(isPow2(writeDepth))   
  require(isPow2(writeWidth))  
  require(isPow2(readWidth))  
  require(readWidth >= writeWidth)
}

object BramConfig {
  val map = mutable.Map[String, BramConfig]()

 
}

class BramInterface(_writeWidth : Int, _writeDepth : Int, _readWidth : Int) extends Bundle {
  val config = new BramConfig {
    val writeWidth = _writeWidth
    val writeDepth = _writeDepth
    val readWidth = _readWidth
    val totalSpace = writeWidth * writeDepth
    val readDepth = totalSpace / readWidth 
  }
  

  val writeEnable = Output(Bool())
  val writeAddr = Output(UInt(log2Ceil(config.writeDepth).W))
  val writeData = Output(UInt(config.writeWidth.W))

  val readAddr = Output(UInt(log2Ceil(config.readDepth).W))
  val readData = Input(UInt(config.readWidth.W))
}

class SimpleDualPortBram(name : String, config : BramConfig) extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val addra = Input(UInt(config.writeAddrWidth.W))
    val dina = Input(UInt(config.writeWidth.W))
    val wea = Input(Bool())

    val clkb = Input(Clock())
    val addrb = Input(UInt(config.readAddrWidth.W))
    val doutb = Output(UInt(config.readWidth.W))
  })

  override def desiredName = name
}



class Bram(name : String, writeWidth : Int, writeDepth : Int, readWidth : Int) extends Module {
  val io = IO(Flipped(new BramInterface(writeWidth, writeDepth, readWidth)))
  val config = io.config

  if (GenConfig.verilator) {
    val mem = SyncReadMem(config.writeDepth, UInt(config.writeWidth.W))

    val readNum = config.readNum
    val readAddrBegin = io.readAddr << (log2Ceil(readNum)).asUInt

    val readData = Wire(Vec(readNum, UInt(config.writeWidth.W)))

    for (i <- 0 until readNum) {
      val addr = readAddrBegin + i.U
      readData(i) := mem.read(addr)
    }

    io.readData := Cat(readData.reverse)
    
    when (io.writeEnable) {
      mem.write(io.writeAddr, io.writeData)
    }
  } else {
    BramConfig.map.update(name, config)

    val bram = Module(new SimpleDualPortBram(name, io.config))
    bram.io.clka := clock
    bram.io.addra := io.writeAddr
    bram.io.dina := io.writeData
    bram.io.wea := io.writeEnable

    bram.io.clkb := clock
    bram.io.addrb := io.readAddr
    io.readData := bram.io.doutb
  }
}


class BramTester(name : String) extends Module {
  val bram = Module(new Bram(name, 32, 1024, 128))

  val io = IO(new Bundle {
    val readData = Output(UInt(bram.config.readWidth.W))
  })

  val writeAddr = RegInit(0.U(32.W))

  writeAddr := writeAddr + 1.U

  bram.io.writeEnable := true.B
  bram.io.writeAddr := writeAddr
  bram.io.writeData := writeAddr

  bram.io.readAddr := writeAddr >> 2.U
  io.readData := bram.io.readData
}
