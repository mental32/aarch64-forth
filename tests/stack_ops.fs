\ EXPECT: FGPQY[Z[acbedf2121
\ Stack shuffles: swap over nip tuck rot -rot 2dup 2drop

: over   ( a b -- a b a )    >r dup r> swap ;
: nip    ( a b -- b )        swap drop ;
: tuck   ( a b -- b a b )    swap over ;
: rot    ( a b c -- b c a )  >r swap r> swap ;
: -rot   ( a b c -- c a b )  swap >r swap r> ;
: 2dup   ( a b -- a b a b )  over over ;
: 2drop  ( a b -- )          drop drop ;


: boot
  70 71 swap emit emit              \ FG
  80 81 over emit emit              \ PQ
  88 89 nip emit                    \ Y
  90 91 tuck emit emit emit         \ [Z[
  97 98 99 rot emit emit emit       \ bca
  100 101 102 -rot emit emit emit   \ fde
  49 50 2dup emit emit emit emit    \ 2121
  55 56 2drop
  bye
;
