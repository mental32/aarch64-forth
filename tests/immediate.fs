\ EXPECT: 2

\ Prove compile-time execution in compile.pl:
\  - 'mark' is an IMMEDIATE word that bumps &var_cell.
\  - We call 'mark' twice at top level.
\  - boot just reads &var_cell and prints it.
\  Expected: compiled image has var_cell init = 2, so output '2'.

: 1+     ( n -- n+1 ) 1 + ;

: immediate  \ ( -- )
  \ Mark the most recently defined word as IMMEDIATE by
  \ setting the low bit in its header flags byte.
  &latest @ 8 +        \ addr of flags byte
  dup c@ 1 or          \ set bit 0
  swap c! ;

: mark  \ ( -- )
  &var_cell @ 1+ &var_cell ! ;
immediate

: boot
  mark
  mark
  &var_cell @ 48 + emit  \ print '1'
  bye
;
