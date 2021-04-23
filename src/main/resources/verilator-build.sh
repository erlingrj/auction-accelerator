#!/bin/sh

# requires a recent version of verilator, e.g. 3.916
VERILATOR_SRC_DIR="/usr/share/verilator/include"

# call verilator to translate verilog to C++
verilator -Iother-verilog --cc TesterWrapper.v -Wno-assignin -Wno-fatal -Wno-lint -Wno-style -Wno-COMBDLY -Wno-STMTDLY --Mdir verilated --trace
# if verilator freezes while executing, consider adding +define+SYNTHESIS=1
# to the cmdline here. this will disable the Chisel printfs though.

# add verilated.cpp from source dirs
cp -f $VERILATOR_SRC_DIR/verilated.cpp .
cp -f $VERILATOR_SRC_DIR/verilated_vcd_c.cpp .
# compile everything
g++ -std=c++17 $@ -I$VERILATOR_SRC_DIR -Iauction-cpp -Iverilated *.cpp auction-cpp/AuctionSolver.cpp auction-cpp/utils.cpp verilated/*.cpp -o verilatedtesterwrapper -lstdc++fs
