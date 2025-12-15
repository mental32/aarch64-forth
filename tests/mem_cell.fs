\ EXPECT: AB
\ Memory cell ops: ! @ +!

: rot    ( a b c -- b c a )  >r swap r> swap ;
: +!     ( n addr -- )       dup @ rot + swap ! ;

: boot
  65 &var_cell !          \ A
  &var_cell @ emit        \ A
  1 &var_cell +!          \ increment to B
  &var_cell @ emit        \ B
  bye
;
