\ EXPECT: [   ]
\ space / spaces

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

: if     \ ( -- orig )
  &xt_zero_branch ,   \ compile 0branch
  here                 \ placeholder address
  0 ,                  \ reserve slot
; immediate

: then   \ ( orig -- )
  here swap ! ; immediate

: else   \ ( orig -- orig2 )
  &xt_branch ,         \ compile jump over ELSE
  here                 \ new placeholder for AFTER ELSE
  0 ,                  \ reserve slot
  swap                 \ bring old orig on top
  here swap !          \ patch old orig to ELSE start
; immediate

: 1-     ( n -- n-1 ) 1 - ;

: space   32 emit ;
: spaces  \ ( n -- )
  dup 0= IF drop EXIT THEN
  space 1- spaces ;

: boot
  91 emit                   \ '['
  3 spaces
  93 emit                   \ ']'
  bye
;
