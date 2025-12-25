\ EXPECT: 2

\ Prove compile-time execution in eval-to-asm.pl:
\  - 'mark' is an IMMEDIATE word that bumps &var_cell.
\  - We call 'mark' twice at top level.
\  - boot just reads &var_cell and prints it.
\  Expected: compiled image has var_cell init = 2, so output '2'.

include prelude.fth

: mark  ( -- )
  &var_cell @ 1 + &var_cell ! ;
immediate

: boot
  mark
  mark
  &var_cell @ 48 + emit  \ print '1'
  bye
;
