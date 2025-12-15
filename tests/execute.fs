\ EXPECT: X
\ INPUT: foo

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

: foo 88 emit ;

: dump-dict
  &latest @
  label:loop
    dup 0= if drop exit then
    dup 8 + c@ emit          \ flags (unused)
    dup 9 + c@ emit          \ name length as char
    dup 10 + c@ emit         \ first char
    @                        \ follow link
    >loop
  ;

: boot
  \ Preload buffer with "foo"
  &input_buf
    dup 102 swap c!          \ f
    dup 111 swap 1 + c!      \ o
    dup 111 swap 2 + c!      \ o
  drop
  3 &input_len !
  0 &input_pos !

  &input_buf &input_len @      \ addr len (skip read-line)
  find-name if
    execute                    \ run xt of foo
  else
    drop drop
    63 emit                    \ '?'
  then
  bye
;
