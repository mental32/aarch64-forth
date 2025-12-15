\ EXPECT: Y
\ INPUT: dup

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

: read-line-core     \ ( -- addr len )
  &input_buf &input_len @ ;

: boot
  \ Preload buffer with "dup"
  &input_buf
    dup 100 swap c!                \ 'd'
    dup 117 swap 1 + c!            \ 'u'
    dup 112 swap 2 + c!            \ 'p'
  drop
  3 &input_len !
  0 &input_pos !

  read-line-core                    \ addr len
  find-name if
    drop                       \ drop xt
    89 emit                    \ 'Y'
  else
    drop drop                  \ xt flag
    78 emit                    \ 'N'
  then
  bye
;
