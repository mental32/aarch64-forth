\ EXPECT: xxx
\ BEGIN / UNTIL loop

: here   \ ( -- addr )
  &dict_here @ ;
: allot  \ ( n -- )
  &dict_here @ + &dict_here ! ;
: ,      \ ( x -- )
  &dict_here @
  dup 8 + &dict_here !
  ! ;
: c,     \ ( c -- )
  &dict_here @
  dup 1 + &dict_here !
  c! ;



: immediate  \ ( -- )
  \ Mark the most recently defined word as IMMEDIATE by
  \ setting the low bit in its header flags byte.
  &latest @ 8 +        \ addr of flags byte
  dup c@ 1 or          \ set bit 0
  swap c! ;

: begin  \ ( -- dest )
  here ; immediate

: until  \ ( dest -- )
  &xt_zero_branch , ,
; immediate

: boot
  3 &var_cell !
  BEGIN
    120 emit                 \ x
    &var_cell @ 1 - dup &var_cell !
    &var_cell @ 0=
  UNTIL
  bye
;
