\ EXPECT: A
\ Byte ops: c! c@
include prelude.fth
: boot
  64 &var_byte c!         \ '@'
  &var_byte c@ 1+ emit    \ 'A'
  bye
;
