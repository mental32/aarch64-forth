\ EXPECT: A
\ Test the old bad 2! definition; bounds check should catch the bogus store.

: bad2!
  \ Incorrect 2! that corrupts memory: stores addr at addr+8, then treats x1 as addr
  dup 8 + ! swap ! ;

: boot
  \ Good store/fetch using correct !
  65 &var_cell !          \ var_cell = 'A'
  &var_cell @ emit        \ emit 'A'

  \ Bad 2! should attempt to store 112 at address 111 (nonsense) and trigger bounds
  111 112 &var_cell bad2!

  \ We should not reach here if bounds check fires
  66 emit                  \ 'B'
  bye
;
