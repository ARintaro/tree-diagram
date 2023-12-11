package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._
import AddressException._

class DataMemoryManagementUnit extends Module {
  val io = IO(new Bundle {
    val stb = Input(Bool())
    val dataMode = Input(Bool())
    val vaddr = Input(new VirtualAddress)

    val ack = Output(Bool())
    val entry = Output(new PageTableEntry)
    val exception = Output(new Exception)
  })

  val busIO = IO(BusMasterInterface())

  val ctrlIO = IO(new Bundle {
    val flush = Input(Bool())
  })

  val satp = WireInit(0.U.asTypeOf(new csr_satp_t))
  val priv = WireInit(0.U(2.W))
  BoringUtils.addSink(priv, "globalPrivilegeLevel")
  BoringUtils.addSink(satp, "satp")

  assert(satp.mode === 0.U || !io.stb)

  val List(idle, level1, level2, level3, over) = Enum(5)
  val walkState = RegInit(idle)

  val vaddr = Reg(new VirtualAddress)
  val dataMode = Reg(Bool())
  val walkResult = RegInit(0.U.asTypeOf(new PageTableEntry))

  busIO.dataMode := false.B
  busIO.dataBytesSelect := "b1111".U
  busIO.stb := false.B

  switch(walkState) {
    is(idle) {
      busIO.addr := Cat(satp.ppn, io.vaddr.vpn1, 0.U(2.W))(31, 0)
      when(io.stb) {
        busIO.stb := true.B
        dataMode := io.dataMode
      }
    }
    is(level1) {
      busIO.addr := Cat(satp.ppn, vaddr.vpn1, 0.U(2.W))(31, 0)
      busIO.stb := true.B
      when(!CheckValidRamAddress(busIO.addr)) {
        walkState := idle
        io.ack := true.B
        io.exception.valid := true.B
        io.exception.code := EC_LA_FAULT
      }

      when(busIO.ack) {
        walkState := level2
      }
    }
    is(level2) {
      val walkResultWire = busIO.dataRead.asTypeOf(new PageTableEntry)
      busIO.addr := Cat(
        walkResultWire.ppn1,
        walkResultWire.ppn0,
        vaddr.vpn0,
        0.U(2.W)
      )(31, 0)

      walkResult := walkResultWire

      when(walkResultWire.V) {
        // TODO: Check Valid Address
        busIO.stb := true.B
        walkState := level3
        
      }.otherwise {
        io.ack := true.B
        io.exception.valid := true.B
        io.exception.code := Mux(dataMode, EC_STORE_PF, EC_LOAD_PF)
      }
    }
    is (level3) {
      busIO.addr := Cat(walkResult.ppn1, walkResult.ppn0, vaddr.vpn0, 0.U(2.W))(31, 0)
      busIO.stb := true.B
      when (busIO.ack) {
        walkState := over
      }
    }
    is (over) {
      val walkResultWire = busIO.dataRead.asTypeOf(new PageTableEntry)

      io.ack := true.B
      io.entry := walkResultWire

      when(walkResultWire.V) {
        io.exception.valid := false.B
        io.exception.code := DontCare
      } .otherwise {
        io.exception.valid := true.B
        io.exception.code := Mux(dataMode, EC_STORE_PF, EC_LOAD_PF)
      }
    }
  }

}
