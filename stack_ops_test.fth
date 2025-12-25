\ EXPECT: FGPQY[Z[acbedf2121
\ Stack shuffles: swap over nip tuck rot -rot 2dup 2drop
include prelude.fth
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
