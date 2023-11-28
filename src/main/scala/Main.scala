package core

import chisel3._

import test._

object MakeVivadoVerilog {
  def main(args: Array[String]): Unit = {
    // circt.stage.ChiselStage.emitSystemVerilogFile(new Test, Array("--target-dir", args(0)))
    GenConfig.verilator = false

    circt.stage.ChiselStage.emitSystemVerilogFile(new general_tb, Array("--target-dir",  args(0)))


    for ((name, config) <- BramConfig.map) {
      println(s"===Bram[$name]===")
      config.printMembers()
      println("==================")
    }
  }
  
}

object MakeVerilatorVerilog {
  def main(args: Array[String]): Unit = {
    GenConfig.verilator = true
    circt.stage.ChiselStage.emitSystemVerilogFile(new general_tb, Array("--target-dir", args(0)), Array("-O=debug"))
    
    CacheConfig.icache.Print()
  }
}


object MakeTest {
  def main(args: Array[String]): Unit = {
    GenConfig.verilator = true
    circt.stage.ChiselStage.emitSystemVerilogFile(new LabMaster, Array("--target-dir", args(0)))
    circt.stage.ChiselStage.emitSystemVerilogFile(new SramController, Array("--target-dir", args(0)))
  }
}
