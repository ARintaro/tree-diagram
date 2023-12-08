VIVADO_DIR ?= /mnt/d/ubuntu-tmp/cod23-grp67/thinpad_top.srcs/sources_1/new/
VERILATOR_DIR ?= /mnt/d/ubuntu-tmp/Guaning/general
TEST_DIR ?= ./build/test
CONFIG_DEBUG ?= true

export PATH := $(PATH):$(abspath ./utils)

vivado:
	# mkdir -p $(VIVADO_DIR)
	mill -i TreeDiagram.runMain core.MakeVivadoVerilog $(VIVADO_DIR)

verilator:
	#mkdir -p $(VERILATOR_DIR)
	mill -i TreeDiagram.runMain core.MakeVerilatorVerilog $(VERILATOR_DIR) $(CONFIG_DEBUG)

test:
	mkdir -p $(VERILATOR_DIR)
	mill -i TreeDiagram.runMain core.MakeTest $(TEST_DIR)

compile:
	mill -i TreeDiagram.compile

bsp:
	mill -i mill.bsp.BSP/install


.PHONY: vivado verilator compile bsp

sim:
	$(call git_commit, "sim RTL") # DO NOT REMOVE THIS LINE!!!
	@echo "Write this Makefile by yourself."
