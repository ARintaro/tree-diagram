VIVADO_DIR = ./build/vivado
VERILATOR_DIR = ./build/verilator

export PATH := $(PATH):$(abspath ./utils)

vivado:
	mkdir -p $(VIVADO_DIR)
	mill -i __.runMain core.MakeVivadoVerilog $(VIVADO_DIR)

verilator:
	mkdir -p $(VERILATOR_DIR)
	mill -i __.runMain core.MakeVerilatorVerilog $(VERILATOR_DIR)

compile:
	mill -i __.compile

bsp:
	mill -i mill.bsp.BSP/install


.PHONY: vivado verilator compile bsp

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by yourself."
