\ EXPECT: X
\ INPUT: foo
include prelude.fth
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
