package core

import chisel3._



object MakeVivadoVerilog {
  def main(args: Array[String]): Unit = {
    // circt.stage.ChiselStage.emitSystemVerilogFile(new Test, Array("--target-dir", args(0)))
    GenConfig.verilator = false

    circt.stage.ChiselStage.emitSystemVerilogFile(new IfuTestTop, Array("--target-dir", args(0)))


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
    circt.stage.ChiselStage.emitSystemVerilogFile(new BramTester("test_bram"), Array("--target-dir", args(0)))

  }
}


object MakeTest {
  def main(args: Array[String]): Unit = {
    GenConfig.verilator = true
    circt.stage.ChiselStage.emitSystemVerilogFile(new PhysicalRegisterFile, Array("--target-dir", args(0)))

  }
}
