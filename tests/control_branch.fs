\ EXPECT: 321
\ branch / 0branch loop

: 1-     ( n -- n-1 ) 1 - ;

: boot
  3 &var_cell !
  label:loop
    &var_cell @ dup 48 + emit   \ emit digit
    1- dup &var_cell !          \ dec, keep copy
    0branch >loop_exit          \ if zero, exit
    branch >loop
  label:loop_exit
    drop
  bye
;
