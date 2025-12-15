\ EXPECT: A
\ Byte ops: c! c@

: 1+     ( n -- n+1 ) 1 + ;

: boot
  64 &var_byte c!         \ '@'
  &var_byte c@ 1+ emit    \ 'A'
  bye
;
