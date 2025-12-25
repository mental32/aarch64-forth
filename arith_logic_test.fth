\ EXPECT: FCACCdPADEE
\ Arithmetic and logic primitives
include prelude.fth

: boot
  2 3 + 65 + emit           \ F
  10 3 - 60 + emit          \ C
  5 negate 70 + emit        \ A
  66 1+ emit                \ C
  68 1- emit                \ C
  20 2* 60 + emit           \ d
  40 2/ 60 + emit           \ P
  1 2 and 65 + emit         \ A
  1 2 or 65 + emit          \ D
  7 3 xor 65 + emit         \ E
  0 invert 70 + emit        \ E
  bye
;
