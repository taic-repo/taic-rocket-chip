# This makefrag is sourced by each board's subdirectory

ROOT_DIR = $(abspath .)
BOOTROOM_DIR = $(ROOT_DIR)/bootrom
ROCKET_DIR = $(ROOT_DIR)/rocket-chip
TAIC_SCALA = $(ROOT_DIR)/taic/src/main/scala
VIVADO_PROJ_DIR = $(ROOT_DIR)/vivado_proj
DEVICE_TREE_DIR = $(ROOT_DIR)/device-tree-xlnx

PROJECT ?= freechips.rocketchip.taic
CONFIG ?= $(PROJECT).TaicConfig

# Initialize submodules and packages
init:
	cd $(ROOT_DIR) && git submodule update --init --recursive
	rm -rf $(ROCKET_DIR)/src/main/scala/taic
	ln -rs $(TAIC_SCALA) $(ROCKET_DIR)/src/main/scala/taic

#--------------------------------------------------------------------
# Testbench generated and simulated by default configurations of rocket chip
# Only support benchmarks
#--------------------------------------------------------------------
TEST_DIR = $(ROOT_DIR)/riscv-tests
BMARKS_DIR = $(TEST_DIR)/build/share/riscv-tests/benchmarks
CASE = ?
ALL_BMARKS = $(wildcard $(TEST_DIR)/build/share/riscv-tests/benchmarks/*.riscv)

emu = $(ROCKET_DIR)/emulator/emulator-$(PROJECT)-$(CONFIG)
emu_debug = $(ROCKET_DIR)/emulator/emulator-$(PROJECT)-$(CONFIG)-debug


test_cycles ?= 100000000

benchmarks: 
	cd $(TEST_DIR) && git submodule update --init --recursive && autoconf && ./configure --prefix=$(TEST_DIR)/build && make && make install

test: benchmarks
	make -C $(ROCKET_DIR)/emulator/ PROJECT=$(PROJECT) CONFIG=$(CONFIG)
	$(foreach case, $(ALL_BMARKS), $(emu) +max-cycles=$(test_cycles) $(case);)

test_one: benchmarks
	make -C $(ROCKET_DIR)/emulator/ PROJECT=$(PROJECT) CONFIG=$(CONFIG)
	$(emu) +max-cycles=$(test_cycles) $(BMARKS_DIR)/$(CASE).riscv

debug_one: benchmarks
	make -C $(ROCKET_DIR)/emulator/ PROJECT=$(PROJECT) CONFIG=$(CONFIG) debug
	$(emu_debug) +max-cycles=$(test_cycles) -vdump.vcd $(BMARKS_DIR)/$(CASE).riscv

-include $(ROCKET_DIR)/Makefrag

#--------------------------------------------------------------------
# Rocket-chip verilog source generation
#--------------------------------------------------------------------

gen_rtl = $(ROCKET_DIR)/emulator/generated-src/$(long_name).v
srams_rtl = $(ROCKET_DIR)/emulator/generated-src/$(long_name).behav_srams.v
other_rtl = $(ROCKET_DIR)/src/main/resources/vsrc/AsyncResetReg.v \
            $(ROCKET_DIR)/src/main/resources/vsrc/EICG_wrapper.v \
            $(ROCKET_DIR)/src/main/resources/vsrc/plusarg_reader.v

rocketchip_rtl = $(VIVADO_PROJ_DIR)/src/hdl/rocketchip.v
	
# Build SoC
$(gen_rtl):
	make -C $(ROCKET_DIR)/emulator/ PROJECT=$(PROJECT) CONFIG=$(CONFIG) MODEL=Top verilog

$(srams_rtl): $(gen_rtl)

$(rocketchip_rtl): $(gen_rtl) $(srams_rtl) $(other_rtl)
	cat $^ > $@

bootrom:
	make -C $(BOOTROOM_DIR)

# Build with custom generator and SoC configurations
build: bootrom $(rocketchip_rtl)

# #--------------------------------------------------------------------
# # Software generation
# #--------------------------------------------------------------------

# %.dtb: %.dts
# 	dtc -I dts -O dtb $< -o $@

#--------------------------------------------------------------------
# Vivado project
#--------------------------------------------------------------------

VIVADO_PROJ = rocket-chip.xpr
DIGILENT_DIR = $(ROOT_DIR)/digilent-vivado-scripts

checkin:
	cd $(VIVADO_PROJ_DIR) && \
	python3 $(DIGILENT_DIR)/git_vivado.py checkin -r . -x $(VIVADO_PROJ_DIR)/proj/$(VIVADO_PROJ)

checkout:
	cd $(VIVADO_PROJ_DIR) && \
	python3 $(DIGILENT_DIR)/git_vivado.py checkout -r . -x $(VIVADO_PROJ_DIR)/proj/$(VIVADO_PROJ)

XSA = system_wrapper.xsa

bitstream:
	cd $(VIVADO_PROJ_DIR) && vivado -mode batch -source xsa.tcl -tclargs $(VIVADO_PROJ_DIR)/proj $(VIVADO_PROJ) $(XSA)

bitbin: bitstream
	cd $(VIVADO_PROJ_DIR)/proj && xsct ../dt_overlay.tcl $(XSA) psu_cortexa53_0 $(DEVICE_TREE_DIR) overlay
	cd $(VIVADO_PROJ_DIR)/proj && dtc -O dtb -o pl.dtbo -b 0 -@ overlay/pl.dtsi
	cd $(VIVADO_PROJ_DIR) && cp bitstream.bif ./proj/ && cd proj && bootgen -image bitstream.bif -arch zynqmp -o ./system_wrapper.bit.bin -w


# gen_dts:
# 	xsct $(COMMON_DIR)/gen_dts.tcl \
# 		/tools/Xilinx/device-tree-xlnx $(VIVADO_PROJ_DIR)/system_wrapper.xsa $(BOARD) dts
# 	cd dts && gcc -I include -E -nostdinc -undef -D__DTS__ -x assembler-with-cpp -o system.dts system-top.dts
# 	dtc -I dts -O dtb -o dts/system.dtb dts/system.dts

#--------------------------------------------------------------------
# Clean rocket-chip project and rocketchip.jar
#--------------------------------------------------------------------

clean:
	rm -f *.log *.jou *.str
	make -C $(ROCKET_DIR)/emulator/ clean
	rm -f $(ROCKET_DIR)/rocketchip.jar

.PHONY: build clean init bootrom test test_one debug_one checkout checkin bitstream bitbin