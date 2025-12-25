ZIG  ?= zig
QEMU ?= qemu-system-aarch64
PERL ?= perl
TEST_TIMEOUT ?= 1s
QEMU_TEST_ARGS ?=
ZIG_CACHE := $(abspath .zig-cache)
ZCC := ZIG_LOCAL_CACHE_DIR=$(ZIG_CACHE) ZIG_GLOBAL_CACHE_DIR=$(ZIG_CACHE) $(ZIG) cc

ELF := forth.elf
BIN := forth.bin
ASM := kernel.S
LDS := linker.ld
GEN := generated.S
SRC ?= kernel.fth

.PHONY: all clean run dumpdtb dts qemu-test eval-test

all: $(ELF) $(BIN)

$(ELF): $(ASM) $(LDS) $(GEN)
	$(ZCC) -target aarch64-freestanding -nostdlib -Wl,-T,$(LDS) $(ASM) $(GEN) -o $@

$(GEN): $(SRC) eval-to-asm.pl
	$(PERL) eval-to-asm.pl $(SRC) > $@

$(BIN): $(ELF)
	# Extract a flat binary (text/rodata) for loaders that expect raw bytes
	/opt/homebrew/opt/llvm/bin/llvm-objcopy -O binary $< $@

run: $(ELF)
	$(QEMU) -machine virt,gic-version=2 -cpu cortex-a72 -nographic \
		-chardev stdio,signal=off,id=char0 -serial chardev:char0 -monitor none \
		-kernel $(ELF)

qemu-test:
	@perl qemu-test.pl $(QEMU_TEST_ARGS)

eval-test:
	@echo "==> arith_logic_test.fth"
	@$(PERL) eval.pl arith_logic_test.fth
	@echo
	@echo "==> bounds_bad2_test.fth"
	@$(PERL) eval.pl bounds_bad2_test.fth
	@echo
	@echo "==> bounds_raw_test.fth"
	@$(PERL) eval.pl bounds_raw_test.fth
	@echo
	@echo "==> comparisons_test.fth"
	@$(PERL) eval.pl comparisons_test.fth
	@echo
	@echo "==> control_begin_until_test.fth"
	@$(PERL) eval.pl control_begin_until_test.fth
	@echo
	@echo "==> control_branch_test.fth"
	@$(PERL) eval.pl control_branch_test.fth
	@echo
	@echo "==> control_if_test.fth"
	@$(PERL) eval.pl control_if_test.fth
	@echo
	@echo "==> double_store_test.fth"
	@$(PERL) eval.pl double_store_test.fth
	@echo
	@echo "==> execute_test.fth"
	@$(PERL) eval.pl execute_test.fth
	@echo
	@echo "==> find_name_test.fth"
	@$(PERL) eval.pl find_name_test.fth
	@echo
	@echo "==> format_spaces_test.fth"
	@$(PERL) eval.pl format_spaces_test.fth
	@echo
	@echo "==> immediate_test.fth"
	@$(PERL) eval.pl immediate_test.fth
	@echo
	@echo "==> key_test.fth"
	@printf "Z" | $(PERL) eval.pl key_test.fth
	@echo
	@echo "==> mem_byte_test.fth"
	@$(PERL) eval.pl mem_byte_test.fth
	@echo
	@echo "==> mem_cell_test.fth"
	@$(PERL) eval.pl mem_cell_test.fth
	@echo
	@echo "==> return_stack_test.fth"
	@$(PERL) eval.pl return_stack_test.fth
	@echo
	@echo "==> stack_ops_test.fth"
	@$(PERL) eval.pl stack_ops_test.fth
	@echo
	@echo "==> string_literal_test.fth"
	@$(PERL) eval.pl string_literal_test.fth
	@echo

# Dump QEMU's device tree blob for the virt machine
dumpdtb:
	$(QEMU) -machine virt,dumpdtb=virt.dtb -cpu cortex-a72 -nographic -display none -S

# Decompile the dumped DTB to a readable DTS (requires dtc)
dts: dumpdtb
	dtc -I dtb -O dts virt.dtb > virt.dts

clean:
	rm -f $(ELF) $(BIN) $(GEN)
