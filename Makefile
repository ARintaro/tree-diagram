VIVADO_DIR = /mnt/d/computer_arc/for_lab2/cod23-zhangzuo21/thinpad_top.srcs/sources_1/new
VERILATOR_DIR = /home/zhang/verilator_testbench
TEST_DIR = ./build/test

export PATH := $(PATH):$(abspath ./utils)

vivado:
	# mkdir -p $(VIVADO_DIR)
	mill -i __.runMain core.MakeVivadoVerilog $(VIVADO_DIR)

verilator:
	#mkdir -p $(VERILATOR_DIR)
	mill -i __.runMain core.MakeVerilatorVerilog $(VERILATOR_DIR)

test:
	mkdir -p $(VERILATOR_DIR)
	mill -i __.runMain core.MakeTest $(TEST_DIR)

compile:
	mill -i __.compile

bsp:
	mill -i mill.bsp.BSP/install


.PHONY: vivado verilator compile bsp

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by yourself."
