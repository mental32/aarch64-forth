\ EXPECT: AB
\ Memory cell ops: ! @ +!
include prelude.fth
: boot
  65 &var_cell !          \ A
  &var_cell @ emit        \ A
  1 &var_cell +!          \ increment to B
  &var_cell @ emit        \ B
  bye
;
