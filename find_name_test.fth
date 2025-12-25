\ EXPECT: Y
\ INPUT: dup
include prelude.fth
: read-line-core     ( -- addr len )
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
