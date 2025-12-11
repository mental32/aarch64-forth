.equ UART0_BASE, 0x09000000 // UART0 base address
.equ UART0_DR, 0x00         // Data Register offset
.equ UART0_FR, 0x18         // Flag Register offset
.equ UART_FR_TXFF, (1 << 5) // TX FIFO Full bit mask
.equ STACK_TOP, 0x41000000  // 16 MB above load addr (default QEMU RAM start 0x40000000)

.text
.global _start

_start:
	ldr x0, =STACK_TOP // LDR (Load Register) - Loads address of STACK_TOP into x0
	mov sp, x0         // MOV (Move) - Copies value from x0 to stack pointer

	ldr x0, =UART0_BASE // LDR - Loads UART0 base address into x0
	adr x1, message     // ADR (Form PC-relative address) - Gets address of message label

print_loop:
	ldrb w2, [x1], #1 // LDRB (Load Register Byte) - Loads byte from [x1], then increments x1 by 1
	cbz  w2, hang     // CBZ (Compare and Branch if Zero) - Branches to 'hang' if w2 is zero
	bl   uart_putc    // BL (Branch with Link) - Calls uart_putc subroutine
	b    print_loop   // B (Branch) - Unconditional branch to print_loop

uart_putc:
wait_tx:
	ldr  w3, [x0, #UART0_FR] // LDR - Loads flag register value into w3
	tst  w3, #UART_FR_TXFF   // TST (Test) - Tests bits in w3 against UART_FR_TXFF mask
	bne  wait_tx             // BNE (Branch if Not Equal) - Branches if TX FIFO is full
	strb w2, [x0, #UART0_DR] // STRB (Store Register Byte) - Stores byte from w2 to UART data register
	ret                      // RET (Return) - Returns from subroutine

hang:
	wfi    // WFI (Wait For Interrupt) - Low power wait state
	b hang // B - Infinite loop

message:
	.asciz "Hello from bare-metal forth!\r\n" // ASCII string with zero terminator
