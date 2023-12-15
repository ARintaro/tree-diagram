Tree Diagram
=======================

A four-issue, out of order RISC-V processor core for computer organization 2023 fall in THU CST.

Supports the RV32I instruction set. Due to time constraints in completing the course assignment, we did not implement a dynamic branch predictor, write-back data cache, or complete support for RISC-V privilege modes (for example, currently, it's even possible to read and write CSR in user mode). 

As of now, the commits on the dev branch are able to boot ucore in Verilator simulation, but they still need to be debugged on an actual FPGA.

A more detailed README and architectural design documents will be supplemented later.