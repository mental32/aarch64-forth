\ EXPECT: po
\ 2! / 2@ round-trip

: two_store  \ ( x1 x2 addr -- )
  >r swap r@ ! r> 8 + ! ;

: two_fetch  \ ( addr -- x1 x2 )
  dup @ swap 8 + @ ;

: boot
  111 112 &var_pair two_store    \ store 'o','p'
  &var_pair two_fetch emit emit  \ op
  bye
;
