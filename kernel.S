// AArch64 Forth Kernel - Complete Forth runtime engine
//
// STANDARDIZED NAMING CONVENTIONS:
//
// Global Symbols:
//   sys_*     - Core system functions (sys_next, sys_check_bounds)
//   hw_*      - Hardware abstraction layer (hw_uart_putc, hw_uart_getc)
//   bss_*     - BSS section symbols (bss_start, bss_end)
//   forth_*   - Forth primitives (forth_lit, forth_exit, forth_dup, etc.)
//   _start    - Special case: linker entry point
//
// Local Labels (function-internal):
//   init_*    - Initialization sequences
//   loop_*    - Loop start points
//   done_*    - Success/completion paths
//   fail_*    - Error/failure paths
//   skip_*    - Skip/bypass paths
//   check_*   - Conditional decision points
//   L_*       - Generic local labels
//
// Constants:
//   ALL_CAPS_WITH_UNDERSCORES - Hardware and system constants

.equ UART0_BASE, 0x09000000  // PL011 UART base (virt)
.equ UART0_DR, 0x00          // UART data register offset
.equ UART0_FR, 0x18          // UART flag register offset
.equ UART0_FR_TXFF, (1 << 5) // transmit FIFO full bit
.equ UART0_FR_RXFE, (1 << 4) // receive FIFO empty bit
.equ UART0_IBRD, 0x24        // integer baud rate divisor
.equ UART0_FBRD, 0x28        // fractional baud rate divisor
.equ UART0_LCRH, 0x2C        // line control register
.equ UART0_LCRH_FEN, (1 << 4)   // FIFO enable
.equ UART0_LCRH_WLEN8, (3 << 5) // 8-bit word length
.equ UART0_CR, 0x30          // control register
.equ UART0_CR_UARTEN, (1 << 0)
.equ UART0_CR_TXE, (1 << 8)
.equ UART0_CR_RXE, (1 << 9)
.equ UART0_RSR, 0x04        // receive status / error clear
.equ UART0_IMSC, 0x38        // interrupt mask set/clear
.equ UART0_RIS, 0x3C         // raw interrupt status
.equ UART0_MIS, 0x40         // masked interrupt status
.equ UART0_ICR, 0x44         // interrupt clear register
.equ UART0_IMSC_RXIM, (1 << 4) // RX interrupt mask bit
.equ UART0_ICR_RXIC, (1 << 4)  // RX interrupt clear bit
.equ GICD_BASE, 0x08000000   // GICv2 distributor base (virt)
.equ GICC_BASE, 0x08010000   // GICv2 CPU interface base (virt)
.equ GICD_CTLR, 0x000        // distributor control
.equ GICD_ISENABLER1, 0x104  // enable interrupts 32-63
.equ GICD_IPRIORITYR_BASE, 0x400 // priority registers base
.equ GICD_ITARGETSR_BASE, 0x800  // target registers base
.equ GICC_CTLR, 0x000        // CPU interface control
.equ GICC_PMR, 0x004         // priority mask
.equ GICC_IAR, 0x00C         // interrupt acknowledge
.equ GICC_EOIR, 0x010        // end of interrupt
.equ GICD_IGROUPR1, 0x084    // group register for IRQs 32-63
.equ IRQ_UART0, 33           // SPI ID for UART0 on virt

.equ PSCI_SYSTEM_OFF, 0x84000008 // PSCI function ID for power off

.equ DSP_SIZE, 1024       // data stack size (bytes)
.equ RSP_SIZE, 1024       // return stack size (bytes)
.equ MEM_BASE, 0x40000000 // base of usable RAM
.equ MEM_SIZE, 0x08000000 // 128MB on QEMU virt
.equ INPUT_SIZE, 256      // line input buffer size

.bss              // uninitialized data section
.global bss_start // expose start of BSS

bss_start: // BSS region start label
	.align 8 // keep BSS 8-byte aligned

dstack:  // data stack storage base
	.skip DSP_SIZE // reserve data stack bytes

dstack_top: // initial data stack pointer

rstack:  // return stack storage base
	.skip RSP_SIZE // reserve return stack bytes

rstack_top: // initial return stack pointer

	.align  8         // align subsequent BSS data
	.global input_buf // expose input buffer symbol

input_buf: // buffer for input characters
	.skip   INPUT_SIZE // reserve input buffer space
	.global input_len  // expose input length symbol

input_len: // length of current input
	.quad   0         // initialize length to zero

	.align 8
uart_byte_ready:
	.quad 0
uart_byte_value:
	.quad 0
	.global input_pos // expose cursor position

input_pos: // index into input_buf
	.quad   0       // initialize cursor to start

	.align 8
dec_buf: // scratch buffer for number formatting (32 bytes)
	.skip   32
dec_buf_end:
	.global bss_end // expose end-of-BSS marker

bss_end: // BSS region end label

	.text                    // executable code section
	.global _start           // entry point symbol (must be _start for linker)
	.global sys_next         // inner interpreter entry
	.global sys_check_bounds // memory bounds helper
	.global hw_uart_putc     // UART transmit helper
	.global hw_uart_getc     // UART receive helper
	.extern latest           // dictionary head from generated.S
	.extern hdr_w_boot       // BOOT word header from generated.S
	.extern entry_thread     // entry point from generated.S

	// Registers:
	// x19 = data stack pointer (down-growing)
	// x20 = return stack pointer (down-growing)
	// x21 = instruction pointer (points to threaded code)
	// x24 = code pointer for current word
	// x25 = CFA (address of current word entry)
	// x18 = UART base
	//
	// Caller-saved discipline for our helpers:
	// - Assume x0–x17 are volatile across any BL (uart_putc/getc, check_bounds, etc.).
	// - Park long-lived state (like the current header pointer) in x19+ or spill/preserve
	//   before calling out, then restore after the call.

_start: // Forth kernel entry point
	// Clear .bss
	ldr x0, =bss_start          // x0 = &bss_start
	ldr x1, =bss_end            // x1 = &bss_end
	sub x2, x1, x0              // x2 = size of BSS in bytes
	cbz x2, init_bss_clear_done // skip clear loop if size is zero
	mov x3, #0                  // constant zero for clearing

init_bss_clear_loop: // loop: zero BSS 8 bytes at a time
	str  x3, [x0], #8        // store zero and bump pointer
	subs x2, x2, #8          // subtract chunk size and set flags
	b.gt init_bss_clear_loop // repeat while bytes remain

init_bss_clear_done: // finished clearing BSS
	// Initialize latest to the boot header emitted in generated.S
	ldr x0, =latest     // x0 = &latest dictionary head
	ldr x1, =hdr_w_boot // x1 = address of BOOT header
	str x1, [x0]        // latest = hdr_w_boot

	// Drop MMU/caches regardless of current EL to ensure flat physical access.
	mrs x0, CurrentEL        // read current exception level
	lsr x0, x0, #2           // now 1, 2, or 3
	cmp x0, #2               // are we at EL2 or higher?
	blt init_el_setup_common // if below EL2, skip EL2 cleanup

	// If we are in EL2 or higher, clear EL2 controls.
	mrs x1, sctlr_el2      // read EL2 system control
	bic x1, x1, #1         // clear M bit (MMU off)
	bic x1, x1, #(1 << 2)  // clear C bit (data cache off)
	bic x1, x1, #(1 << 12) // clear I bit (instruction cache off)
	msr sctlr_el2, x1      // write updated EL2 control
	mov x1, xzr            // zero value for HCR_EL2
	msr hcr_el2, x1        // ensure stage-2 translation off
	isb                    // synchronize context

init_el_setup_common: // common path after EL control setup
	// Install EL1 exception vector table
	ldr x0, =vector_table
	msr VBAR_EL1, x0
	isb                    // ensure vector base is active
	// Bring up minimal GICv2 for UART IRQs
	ldr x0, =GICD_BASE
	mov w1, #0
	str w1, [x0, #GICD_CTLR] // disable distributor during setup
	// Set priority for IRQ_UART0 (byte index = ID)
	ldr w1, =0x00000000
	str w1, [x0, #(GICD_IPRIORITYR_BASE + (IRQ_UART0/4)*4)]
	// Target CPU0 for IRQ_UART0
	ldr w1, =0x01010101
	str w1, [x0, #(GICD_ITARGETSR_BASE + (IRQ_UART0/4)*4)]
	// Put UART0 (IRQ33) into Group1 (non-secure)
	mov w1, #(1 << (IRQ_UART0 - 32))
	str w1, [x0, #GICD_IGROUPR1]
	// Enable UART0 interrupt (IRQ 33 => ISENABLER1 bit1)
	mov w1, #(1 << (IRQ_UART0 - 32))
	str w1, [x0, #GICD_ISENABLER1]
	// Enable distributor
	mov w1, #2              // enable Group1 (we use Group1 interrupts)
	str w1, [x0, #GICD_CTLR]

	// CPU interface setup
	ldr x0, =GICC_BASE
	mov w1, #0xFF
	str w1, [x0, #GICC_PMR]   // unmask priorities
	mov w1, #2
	str w1, [x0, #GICC_CTLR]  // enable CPU interface for Group1

	// Clear UART RX interrupt and unmask at device
	movz x18, #(UART0_BASE & 0xFFFF), lsl #0
	movk x18, #((UART0_BASE >> 16) & 0xFFFF), lsl #16
	movk x18, #((UART0_BASE >> 32) & 0xFFFF), lsl #32
	movk x18, #((UART0_BASE >> 48) & 0xFFFF), lsl #48
	// UART base now in x18; configure baud + frame
	mov w0, #0
	str w0, [x18, #UART0_CR] // disable before config
	mov w0, #13
	str w0, [x18, #UART0_IBRD]
	mov w0, #2
	str w0, [x18, #UART0_FBRD]
	mov w0, #(UART0_LCRH_FEN | UART0_LCRH_WLEN8)
	str w0, [x18, #UART0_LCRH]
	mov w0, #UART0_ICR_RXIC
	str w0, [x18, #UART0_ICR]
	mov w0, #(UART0_CR_UARTEN | UART0_CR_RXE | UART0_CR_TXE)
	str w0, [x18, #UART0_CR]
	mov w0, #UART0_IMSC_RXIM
	str w0, [x18, #UART0_IMSC]
	// Reset UART byte ready flag
	ldr x0, =uart_byte_ready
	str xzr, [x0]
	ldr x19, =dstack_top // data stack pointer
	ldr x20, =rstack_top // return stack pointer
	mov sp, x20          // use return stack as CPU SP

	// Be defensive: ensure MMU/caches off so physical addresses work for our flat mapping.
	mrs x0, sctlr_el1      // read EL1 system control
	bic x0, x0, #1         // clear M bit (MMU off)
	bic x0, x0, #(1 << 2)  // clear C bit (data cache off)
	bic x0, x0, #(1 << 12) // clear I bit (instruction cache off)
	orr x0, x0, #(1 << 1)  // enable alignment fault checking (A bit)
	msr sctlr_el1, x0      // write updated EL1 control
	isb                    // ensure changes take effect
	msr daifclr, #0x2      // enable IRQs (leave FIQ/SError masked)

	ldr x21, =entry_thread // start threaded execution at entry_thread
	b   sys_next           // jump into inner interpreter

	// NEXT: inner interpreter
sys_next: // threaded code dispatcher
	ldr x25, [x21], #8 // x25 = next CFA; advance IP
	ldr x24, [x25]     // x24 = code pointer stored in CFA

	br  x24            // branch to code for current word

// --- Minimal exception handling for EL1 ---
// We keep a simple vector table that funnels all sync exceptions to a logger
// that prints ESR/ELR/FAR then powers off via forth_bye. Other exception
// classes just spin.
.align 11
vector_table:
// EL1t
	b exc_sync_el1
	b exc_irq_sp_el1
	b exc_fiq_sp_el1
	b exc_serr_sp_el1
// EL1h
	b exc_sync_el1
	b exc_irq_sp_el1
	b exc_fiq_sp_el1
	b exc_serr_sp_el1
// EL0_64
	b exc_sync_el0_64
	b exc_irq_sp_el0
	b exc_fiq_sp_el0
	b exc_serr_sp_el0
// EL0_32 (unused)
	b exc_sync_el0_32
	b exc_irq_sp_el0
	b exc_fiq_sp_el0
	b exc_serr_sp_el0

// Hex helpers for exception logging (do not touch the Forth stacks)
exc_emit_char: // w2 = byte to emit
	b   hw_uart_putc

exc_emit_hex_nibble: // w2=nibble (0-15) -> emits ascii
	cmp w2, #10
	b.lt exc_emit_hex_dec
	add w2, w2, #'A' - 10
	b   exc_emit_char
exc_emit_hex_dec:
	add w2, w2, #'0'
	b   exc_emit_char

exc_emit_hex64: // x0 = value, x1/x2 scratch
	mov x1, #16
exc_hex64_loop:
	lsr x2, x0, #60        // grab top nibble
	and w2, w2, #0xF
	bl  exc_emit_hex_nibble
	lsl x0, x0, #4
	subs x1, x1, #1
	b.ne exc_hex64_loop
	ret

exc_emit_str: // x0=address, x1=len
	cbz x1, exc_emit_str_done
exc_emit_str_loop:
	ldrb w2, [x0], #1
	bl   exc_emit_char
	subs x1, x1, #1
	b.ne exc_emit_str_loop
exc_emit_str_done:
	ret

// Debug helper: given x0 = xt address (CFA), walk dictionary to find matching
// header and emit its name. Clobbers x0-x8.
exc_emit_name_for_xt:
	ldr x10, =latest
	ldr x10, [x10]      // x10 = current header
	mov x9, #256        // safety limit
exc_name_loop:
	subs x9, x9, #1
	beq  exc_name_done
	cbz  x10, exc_name_done
	ldrb w3, [x10, #8]  // flags (unused)
	ldrb w4, [x10, #9]  // name length
	add  x5, x10, #10   // name pointer
	// compute xt address for this header: align past name
	add x6, x5, x4
	add x6, x6, #7
	and x6, x6, #~7     // x6 = xt label address
	cmp x0, x6
	beq exc_name_emit
	ldr x10, [x10]      // follow link
	b   exc_name_loop

exc_name_emit:
	// emit name (x5=name ptr, w4=len)
	mov x0, x5
	mov x1, x4
	bl  exc_emit_str
	b   exc_name_done

exc_name_done:
	ret

exc_log_and_bye:
	// On entry: ESR in x0, ELR in x1, FAR in x2
	stp x29, x30, [sp, #-16]!
	stp x5, x6, [sp, #-16]!
	stp x7, x8, [sp, #-16]!
	mov x29, sp

	// Preserve incoming registers we need to print
	mov x5, x0 // ESR
	mov x6, x1 // ELR
	mov x7, x2 // FAR

	// Print header
	ldr x3, =exc_hdr
	ldr w4, =exc_hdr_len
	mov x0, x3
	mov x1, x4
	bl  exc_emit_str

	// ESR
	ldr x3, =esr_str
	ldr w4, =esr_str_len
	mov x0, x3
	mov x1, x4
	bl  exc_emit_str
	mov x0, x5 // ESR
	bl  exc_emit_hex64

	// ELR
	ldr x3, =elr_str
	ldr w4, =elr_str_len
	mov x0, x3
	mov x1, x4
	bl  exc_emit_str
	mov x0, x6 // ELR
	bl  exc_emit_hex64

	// FAR
	ldr x3, =far_str
	ldr w4, =far_str_len
	mov x0, x3
	mov x1, x4
	bl  exc_emit_str
	mov x0, x7 // FAR
	bl  exc_emit_hex64

	// Trailing newline
	ldr x3, =nl_str
	ldr w4, =nl_str_len
	mov x0, x3
	mov x1, x4
	bl  exc_emit_str

	ldp x7, x8, [sp], #16
	ldp x5, x6, [sp], #16
	ldp x29, x30, [sp], #16
	b   forth_bye

exc_sync_el1:
	// Capture ESR/ELR/FAR then log and power off
	mrs x0, esr_el1
	mrs x1, elr_el1
	mrs x2, far_el1
	b   exc_log_and_bye

// IRQ handler for EL1: service UART RX, minimal save/restore
exc_irq_sp_el1:
	sub sp, sp, #48
	stp x0, x1, [sp]
	stp x2, x3, [sp, #16]
	stp x30, x9, [sp, #32]

	// Acknowledge interrupt
	ldr x9, =GICC_BASE
	ldr w10, [x9, #GICC_IAR]
	and w0, w10, #0x3ff // extract IRQ ID
	// Check if this is UART0
	cmp w0, #IRQ_UART0
	bne irq_skip_uart
	// Read byte from UART and store
	ldr x18, =UART0_BASE
	ldr w1, [x18, #UART0_DR]
	and w1, w1, #0xFF
	ldr x2, =uart_byte_value
	strb w1, [x2]
	ldr x2, =uart_byte_ready
	mov w1, #1
	str w1, [x2]
	// Clear UART interrupt
	mov w1, #UART0_ICR_RXIC
	str w1, [x18, #UART0_ICR]
irq_skip_uart:
	// EOI
	str w10, [x9, #GICC_EOIR]

	ldp x30, x9, [sp, #32]
	ldp x2, x3, [sp, #16]
	ldp x0, x1, [sp], #48
	eret

// Placeholder handlers: just spin forever (could be extended later)
exc_sync_el0_64: b .
exc_sync_el0_32: b .
exc_irq_sp_el0:  b .
exc_fiq_sp_el1:  b .
exc_serr_sp_el1: b .
exc_fiq_sp_el0:  b .
exc_serr_sp_el0: b .

// Strings for exception messages
.align 3
exc_hdr:     .ascii "[EXC] sync exception\\r\\n"
exc_hdr_len = . - exc_hdr
esr_str:     .ascii "ESR="
esr_str_len = . - esr_str
elr_str:     .ascii " ELR="
elr_str_len = . - elr_str
far_str:     .ascii " FAR="
far_str_len = . - far_str
nl_str:      .ascii "\\r\\n"
nl_str_len = . - nl_str

.p2align 2

	// Helpers (not threaded)
	// Bounds check: x0=addr, x1=size. Assumes MEM_BASE..MEM_BASE+MEM_SIZE is valid RAM.
	// On failure, call bye (power off) rather than trap.
sys_check_bounds: // validate address range and alignment
	// lower bound
	mov x2, #MEM_BASE // x2 = low end of valid memory
	cmp x0, x2        // compare start address with base
	blo fail_bounds   // if below base, fail

	// upper bound (inclusive end = addr+size-1)
	add x3, x0, x1                 // x3 = addr + size
	sub x3, x3, #1                 // inclusive end address
	mov x4, #(MEM_BASE + MEM_SIZE) // x4 = first address past valid range
	cmp x3, x4                     // compare end against limit
	bhs fail_bounds                // if >= limit, fail

	// natural alignment for cell writes
	cmp  x1, #8         // check if size is one cell
	bne  done_bounds_ok // if not, skip alignment check
	tst  x0, #7         // test 8-byte alignment of addr
	b.ne fail_bounds    // misaligned: fail

done_bounds_ok: // bounds and alignment OK
	ret // return to caller

fail_bounds: // out-of-bounds or misaligned
	b forth_bye // tail-call bye (power off)

hw_uart_putc: // write byte in w2 to UART
	ldr  x18, =UART0_BASE
loop_uart_wait_tx: // wait until TX FIFO has space
	ldr  w3, [x18, #UART0_FR] // read UART flag register
	tst  w3, #UART0_FR_TXFF   // test transmit FIFO full bit
	bne  loop_uart_wait_tx    // loop while TX FIFO is full
	strb w2, [x18, #UART0_DR] // write byte to UART data register
	ret                       // return to caller

hw_uart_getc: // read a byte from UART
	ldr  x18, =UART0_BASE
	ldr  w0, [x18, #UART0_DR] // read received word (no FR check)
	and  w0, w0, #0xFF        // keep low byte
	ret                       // return to caller

forth_uart_poll: // ( -- u ) blocking read using hw_uart_getc
	bl   hw_uart_getc
	str  x0, [x19, #-8]!
	b    sys_next

// Primitive word names (label -> Forth name):
// prim: forth_lit lit
// prim: forth_exit exit
// prim-skip: forth_docol
// prim: forth_dodoes dodoes
// prim: forth_do_does do-does
// prim: forth_execute execute
// prim: forth_emit emit
// prim: forth_key key
// prim: forth_next_token next-token
// prim: forth_find_name find-name
// prim: forth_parse_num parse-num
// prim: forth_type type
// prim: forth_bye bye
// prim: forth_store !
// prim: forth_uart_poll uart-poll
// prim: forth_dup dup
// prim: forth_drop drop
// prim: forth_swap swap
// prim: forth_to_r >r
// prim: forth_r_from r>
// prim: forth_r_fetch r@
// prim: forth_plus +
// prim: forth_minus -
// prim: forth_div /
// prim: forth_mod mod
// prim: forth_dot .
// prim: forth_or_op or
// prim: forth_and_op and
// prim: forth_xor_op xor
// prim: forth_invert invert
// prim: forth_two_slash 2/
// prim: forth_fetch @
// prim: forth_c_fetch c@
// prim: forth_c_store c!
// prim: forth_zero_equal 0=
// prim: forth_zero_less 0<
// prim: forth_branch branch
// prim: forth_zero_branch 0branch
// Forth primitive implementations
.global forth_lit
.global forth_exit
.global forth_docol
.global forth_dodoes
.global forth_do_does
.global forth_execute
.global forth_emit
.global forth_key
.global forth_next_token
.global forth_find_name
.global forth_parse_num
.global forth_type
.global forth_bye
.global forth_store
.global forth_uart_poll
.global forth_dup
.global forth_drop
.global forth_swap
.global forth_to_r
.global forth_r_from
.global forth_r_fetch
.global forth_plus
.global forth_minus
.global forth_div
.global forth_mod
.global forth_dot
.global forth_or_op
.global forth_and_op
.global forth_xor_op
.global forth_invert
.global forth_two_slash
.global forth_fetch
.global forth_c_fetch
.global forth_c_store
.global forth_zero_equal
.global forth_zero_less
.global forth_branch
.global forth_zero_branch

forth_lit: // ( -- n )
	ldr x0, [x21], #8
	str x0, [x19, #-8]!
	b   sys_next

forth_exit: // ( -- ) return from colon word
	ldr x21, [x20], #8
	b   sys_next

forth_execute: // ( xt -- )
	ldr x25, [x19], #8 // xt/CFA
	ldr x24, [x25]     // code pointer
	br  x24

forth_docol: // colon definition entry
	str x21, [x20, #-8]! // push caller IP
	add x21, x25, #8     // parameter field starts right after CFA
	b   sys_next

forth_dodoes: // ( -- addr ) run created word
	add x0, x25, #16
	str x0, [x19, #-8]!
	ldr x1, [x25, #8]
	cbz x1, sys_next
	str x21, [x20, #-8]!
	mov x21, x1
	b   sys_next

forth_do_does: // ( addr -- ) patch latest and exit defining word
	ldr x0, [x19], #8
	ldr x1, =latest
	ldr x1, [x1]
	cbz x1, sys_next
	ldrb w2, [x1, #9]
	add x3, x1, #10
	add x3, x3, x2
	add x3, x3, #7
	and x3, x3, #~7
	str x0, [x3, #8]
	b   forth_exit

forth_emit: // ( char -- )
	ldr w2, [x19], #8
	bl  hw_uart_putc
	b   sys_next

forth_key: // ( -- char )
	// wait for byte delivered by IRQ; fall back to polling after a timeout
	ldr x1, =uart_byte_ready
	ldr x2, =uart_byte_value
	str xzr, [x1]          // clear ready flag
	mov x3, #0x10000       // timeout counter (shorter to avoid hangs)
wait_key_irq:
	ldr w0, [x1]
	cbnz w0, got_key_irq
	// check UART directly as fallback
	ldr  w4, [x18, #UART0_FR]
	tst  w4, #UART0_FR_RXFE
	beq  got_key_poll
	subs x3, x3, #1
	cbz  x3, poll_uart_now
	b    wait_key_irq
got_key_poll:
	ldr  w0, [x18, #UART0_DR]
	and  w0, w0, #0xFF
	str  x0, [x19, #-8]!
	b    sys_next
poll_uart_now:
	b    forth_bye
wait_key_sleep:
	wfi
	b    wait_key_irq
got_key_irq:
	ldrb w0, [x2]
	str  x0, [x19, #-8]!
	b    sys_next



forth_next_token:
	ldr x3, =input_pos
	ldr x2, [x3]
	ldr x4, =input_len
	ldr x1, [x4]
	ldr x0, =input_buf
	cmp x2, x1
	bge L_next_token_no_tok

loop_next_token_skip_ws:
	cmp  x2, x1
	bge  L_next_token_no_tok
	ldrb w5, [x0, x2]
	cmp  w5, #' '
	bgt  L_next_token_start
	add  x2, x2, #1
	b    loop_next_token_skip_ws

L_next_token_start:
	mov x6, x2

loop_next_token_char:
	cmp  x2, x1
	bge  done_next_token
	ldrb w5, [x0, x2]
	cmp  w5, #' '
	ble  done_next_token
	add  x2, x2, #1
	b    loop_next_token_char

done_next_token:
	str x2, [x3]
	sub x7, x2, x6
	add x8, x0, x6
	str x8, [x19, #-8]!
	str x7, [x19, #-8]!
	mov x9, #-1
	str x9, [x19, #-8]!
	b   sys_next

L_next_token_no_tok:
	str x1, [x3]
	mov x8, #0
	str x8, [x19, #-8]!
	str x8, [x19, #-8]!
	str x8, [x19, #-8]!
	b   sys_next

forth_find_name:
	ldr x1, [x19], #8
	ldr x0, [x19], #8
	ldr x2, =latest
	ldr x2, [x2]
	mov x10, x2
	mov x9, #256

loop_find_name:
	subs x9, x9, #1
	beq  fail_find_name
	cbz  x10, fail_find_name
	ldrb w3, [x10, #8]
	ldrb w4, [x10, #9]
	cmp  x1, x4
	bne  L_find_name_next
	add  x5, x10, #10
	mov  x6, #0

loop_find_name_cmp:
	cmp  x6, x1
	bge  done_find_name_match
	ldrb w7, [x0, x6]
	ldrb w8, [x5, x6]
	cmp  w7, w8
	bne  L_find_name_next
	add  x6, x6, #1
	b    loop_find_name_cmp

done_find_name_match:
	add x5, x5, x1
	add x5, x5, #7
	and x5, x5, #~7
	str x5, [x19, #-8]!
	cbz w3, L_find_name_normal
	mov x6, #1
	b   L_find_name_store_flag

L_find_name_normal:
	mov x6, #-1

L_find_name_store_flag:
	str x6, [x19, #-8]!
	b   sys_next

L_find_name_next:
	ldr x10, [x10]
	b   loop_find_name

fail_find_name:
	mov x6, #0
	str x6, [x19, #-8]!
	str x6, [x19, #-8]!
	b   sys_next

forth_parse_num:
	ldr  x1, [x19], #8
	ldr  x0, [x19], #8
	cbz  x1, fail_parse_num
	ldrb w2, [x0]
	mov  x3, #0
	cmp  w2, #'-'
	bne  L_parse_num_digit
	mov  x3, #1
	add  x0, x0, #1
	sub  x1, x1, #1
	cbz  x1, fail_parse_num

L_parse_num_digit:
	mov x4, #0
	mov x10, #10

loop_parse_num_digit:
	cbz  x1, done_parse_num_digits
	ldrb w2, [x0], #1
	subs w2, w2, #'0'
	blo  fail_parse_num
	cmp  w2, #9
	bhi  fail_parse_num
	mul  x4, x4, x10
	add  x4, x4, x2
	sub  x1, x1, #1
	b    loop_parse_num_digit

done_parse_num_digits:
	cbz x3, done_parse_num_finish
	neg x4, x4

done_parse_num_finish:
	str x4, [x19, #-8]!
	mov x5, #-1
	str x5, [x19, #-8]!
	b   sys_next

fail_parse_num:
	mov x4, #0
	str x4, [x19, #-8]!
	str x4, [x19, #-8]!
	b   sys_next

forth_store:
	ldr x1, [x19], #8
	ldr x0, [x19], #8
	mov x5, x0
	mov x0, x1
	mov x1, #8
	bl  sys_check_bounds
	str x5, [x0]
	b   sys_next

forth_dup:
	ldr x0, [x19]
	str x0, [x19, #-8]!
	b   sys_next

forth_drop:
	add x19, x19, #8
	b   sys_next

forth_swap:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	str x0, [x19, #-8]!
	str x1, [x19, #-8]!
	b   sys_next

forth_to_r:
	ldr x0, [x19], #8
	str x0, [x20, #-8]!
	b   sys_next

forth_r_from:
	ldr x0, [x20], #8
	str x0, [x19, #-8]!
	b   sys_next

forth_r_fetch:
	ldr x0, [x20]
	str x0, [x19, #-8]!
	b   sys_next

forth_plus:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	add x0, x1, x0
	str x0, [x19, #-8]!
	b   sys_next

forth_minus:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	sub x0, x1, x0
	str x0, [x19, #-8]!
	b   sys_next

forth_div: // ( n1 n2 -- n1/n2 ) signed divide, zero divisor -> 0
	ldr x0, [x19], #8 // divisor
	ldr x1, [x19], #8 // dividend
	cbz x0, div_zero
	sdiv x0, x1, x0
	b   div_done
div_zero:
	mov x0, #0
div_done:
	str x0, [x19, #-8]!
	b   sys_next

forth_mod: // ( n1 n2 -- n1 mod n2 ) signed remainder, zero divisor -> 0
	ldr x0, [x19], #8 // divisor
	ldr x1, [x19], #8 // dividend
	cbz x0, mod_zero
	sdiv x2, x1, x0
	msub x0, x2, x0, x1 // x1 - x2*divisor
	b   mod_done
mod_zero:
	mov x0, #0
mod_done:
	str x0, [x19, #-8]!
	b   sys_next

forth_dot: // ( n -- ) print signed decimal
	ldr x0, [x19], #8    // pop n
	cbz x0, dot_zero

	mov x3, #0           // x3 = sign flag (0 -> positive)
	cmp x0, #0
	b.ge dot_abs_ready
	neg x0, x0           // x0 = abs(n)
	mov x3, #1           // remember negative
dot_abs_ready:
	ldr x23, =dec_buf_end // x23 = write ptr (one past end)

dot_build:
	mov x1, #10
	udiv x2, x0, x1      // x2 = n / 10
	msub x4, x2, x1, x0  // x4 = n - q*10 (remainder)
	add x4, x4, #48      // to ASCII
	strb w4, [x23, #-1]! // store and pre-decrement ptr
	mov x0, x2
	cbnz x0, dot_build

	cbz x3, dot_emit_digits
	mov w0, #45          // '-'
	bl  hw_uart_putc

dot_emit_digits:
	ldr x22, =dec_buf_end
dot_emit_loop:
	cmp x23, x22
	b.eq dot_done
	ldrb w0, [x23], #1
	bl   hw_uart_putc
	b    dot_emit_loop

dot_zero:
	mov w0, #48
	bl  hw_uart_putc

dot_done:
	b   sys_next

forth_zero_equal:
	ldr  x0, [x19]
	cmp  x0, #0
	mov  x1, #-1
	mov  x2, #0
	csel x0, x1, x2, eq
	str  x0, [x19]
	b    sys_next

forth_zero_less:
	ldr  x0, [x19]
	cmp  x0, #0
	mov  x1, #-1
	mov  x2, #0
	csel x0, x1, x2, lt
	str  x0, [x19]
	b    sys_next

forth_or_op:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	orr x0, x1, x0
	str x0, [x19, #-8]!
	b   sys_next

forth_and_op:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	and x0, x1, x0
	str x0, [x19, #-8]!
	b   sys_next

forth_xor_op:
	ldr x0, [x19], #8
	ldr x1, [x19], #8
	eor x0, x1, x0
	str x0, [x19, #-8]!
	b   sys_next

forth_invert:
	ldr x0, [x19]
	mvn x0, x0
	str x0, [x19]
	b   sys_next

forth_two_slash:
	ldr x0, [x19]
	asr x0, x0, #1
	str x0, [x19]
	b   sys_next

forth_fetch:
	ldr x0, [x19]
	mov x1, #8
	bl  sys_check_bounds
	ldr x0, [x0]
	str x0, [x19]
	b   sys_next

forth_c_fetch:
	ldr  x0, [x19]
	mov  x1, #1
	bl   sys_check_bounds
	ldrb w0, [x0]
	str  x0, [x19]
	b    sys_next

forth_c_store:
	ldr  x1, [x19], #8
	ldr  x0, [x19], #8
	mov  w5, w0
	mov  x0, x1
	mov  x1, #1
	bl   sys_check_bounds
	strb w5, [x0]
	b    sys_next

forth_type:
	ldr x1, [x19], #8
	ldr x0, [x19], #8

loop_type_char:
	cbz  x1, done_type
	ldrb w2, [x0], #1
	bl   hw_uart_putc
	sub  x1, x1, #1
	b    loop_type_char

done_type:
	b sys_next

forth_bye:
	movz w0, #0x0008
	movk w0, #0x8400, lsl #16
	hvc  #0
	b    forth_bye

forth_branch:
	ldr x0, [x21], #8
	mov x21, x0
	b   sys_next

forth_zero_branch:
	ldr  x0, [x19], #8
	ldr  x1, [x21], #8
	cbnz x0, skip_zero_branch
	mov  x21, x1

skip_zero_branch:
	b sys_next
