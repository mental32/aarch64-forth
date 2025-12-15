ZIG  ?= zig
QEMU ?= qemu-system-aarch64
PERL ?= perl
TEST_TIMEOUT ?= 1s
ZIG_CACHE := $(abspath .zig-cache)
ZCC := ZIG_LOCAL_CACHE_DIR=$(ZIG_CACHE) ZIG_GLOBAL_CACHE_DIR=$(ZIG_CACHE) $(ZIG) cc

ELF := forth.elf
BIN := forth.bin
ASM := kernel.S
LDS := linker.ld
GEN := generated.S
SRC ?= program.fs

.PHONY: all clean run dumpdtb dts test

all: $(ELF) $(BIN)

$(ELF): $(ASM) $(LDS) $(GEN)
	$(ZCC) -target aarch64-freestanding -nostdlib -Wl,-T,$(LDS) $(ASM) $(GEN) -o $@

$(GEN): $(SRC) compile.pl
	$(PERL) compile.pl $(SRC) > $@

$(BIN): $(ELF)
	# Extract a flat binary (text/rodata) for loaders that expect raw bytes
	/opt/homebrew/opt/llvm/bin/llvm-objcopy -O binary $< $@

run: $(ELF)
	$(QEMU) -machine virt,gic-version=2 -cpu cortex-a72 -nographic \
		-chardev stdio,signal=off,id=char0 -serial chardev:char0 -monitor none \
		-kernel $(ELF)

test:
	@perl test.pl

# Dump QEMU's device tree blob for the virt machine
dumpdtb:
	$(QEMU) -machine virt,dumpdtb=virt.dtb -cpu cortex-a72 -nographic -display none -S

# Decompile the dumped DTB to a readable DTS (requires dtc)
dts: dumpdtb
	dtc -I dtb -O dts virt.dtb > virt.dts

clean:
	rm -f $(ELF) $(BIN) $(GEN)
