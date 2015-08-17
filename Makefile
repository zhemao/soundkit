SBT=sbt
OUTPUT_DIR=generated-src
CONFIG=DefaultConfig
EMU_FLAGS=--compile --genHarness --test --backend c \
	--minimumCompatibility 3.0.0 --debug --vcd \
	--targetDir $(OUTPUT_DIR) --configInstance soundkit.$(CONFIG)
FPGA_FLAGS=--compile --backend fpga \
	--minimumCompatibility 3.0.0 --noInlineMem \
	--targetDir $(OUTPUT_DIR) --configInstance soundkit.$(CONFIG)

toplevel_modules := Codec I2CConfig
generated_verilog := $(addprefix $(OUTPUT_DIR)/,$(addsuffix .$(CONFIG).v,$(toplevel_modules)))

verilog: $(generated_verilog)

$(OUTPUT_DIR)/Codec.$(CONFIG).v: src/main/scala/codec.scala
	$(SBT) "run-main soundkit.CodecMain $(FPGA_FLAGS)"

$(OUTPUT_DIR)/I2CConfig.$(CONFIG).v: src/main/scala/codec.scala
	$(SBT) "run-main soundkit.I2CMain $(FPGA_FLAGS)"

i2c-test: src/main/scala/i2c.scala
	$(SBT) "run-main soundkit.I2CMain $(EMU_FLAGS)"

codec-test: src/main/scala/codec.scala
	$(SBT) "run-main soundkit.CodecMain $(EMU_FLAGS)"
