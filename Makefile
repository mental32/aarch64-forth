ZIG  ?= zig
QEMU ?= qemu-system-aarch64

ELF := forth.elf
BIN := forth.bin
ASM := forth.S
LDS := linker.ld

.PHONY: all clean run

all: $(ELF) $(BIN)

$(ELF): $(ASM) $(LDS)
	$(ZIG) cc -target aarch64-freestanding -nostdlib -Wl,-T,$(LDS) $(ASM) -o $@

$(BIN): $(ELF)
	# Extract a flat binary (text/rodata) for loaders that expect raw bytes
	/opt/homebrew/opt/llvm/bin/llvm-objcopy -O binary $< $@

run: $(ELF)
	$(QEMU) -machine virt -cpu cortex-a72 -nographic -kernel $(ELF)

clean:
	rm -f $(ELF) $(BIN) hello_zig.elf hello.bin hello.o hello_uart.o hello_uart.bin
