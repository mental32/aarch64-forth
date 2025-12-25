ENTRY(_start)

SECTIONS
{
  . = 0x40000000;

  .text : {
    *(.text*)
    *(.rodata*)
  }

  . = ALIGN(16);
  .data : {
    *(.data*)
  }

  .bss : {
    *(.bss*)
    *(COMMON)
  }
}

