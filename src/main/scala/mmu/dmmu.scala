package core

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import InsConfig.ExceptionCode._
import BusConfig._
import MemoryManagementConstants._
import PrivilegeLevel._

class DataMemoryManagementUnit extends Module {
    val io = IO(new Bundle {
        val bus = BusMasterInterface()
    })        

    val ctrlIO = IO(new Bundle{
        val flush = Input(Bool())
    })

    io.bus.master_turn_off()
}