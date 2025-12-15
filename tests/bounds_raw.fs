\ EXPECT: A
\ Simple bounds-check test: do a valid store/fetch, then an invalid store to 0 triggers bye.

: boot
  \ Good store/fetch
  65 &var_cell !          \ var_cell = 'A'
  &var_cell @ emit        \ expect 'A'

  \ Deliberate bad store to address 0 (out of MEM_BASE range) should trigger bye
  123 0 !

  \ We should never reach here if bounds check works
  66 emit
  bye
;
