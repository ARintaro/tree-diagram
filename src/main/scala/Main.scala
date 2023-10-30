package core

object MakeVivadoVerilog {
  def main(args: Array[String]): Unit = {
    circt.stage.ChiselStage.emitSystemVerilogFile(new Test, Array("--target-dir", args(0)))
  }
  
}

object MakeVerilatorVerilog {
  def main(args: Array[String]): Unit = {
    circt.stage.ChiselStage.emitSystemVerilogFile(new GCD, Array("--target-dir", args(0)))
  }
}
