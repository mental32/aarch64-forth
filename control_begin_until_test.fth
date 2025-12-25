\ EXPECT: xxx
\ BEGIN / UNTIL loop
include prelude.fth
: boot
  3 &var_cell !
  BEGIN
    120 emit                 \ x
    &var_cell @ 1 - dup &var_cell !
    &var_cell @ 0=
  UNTIL
  bye
;
