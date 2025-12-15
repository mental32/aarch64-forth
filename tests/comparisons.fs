\ EXPECT: EFSTESS
\ Comparison primitives: 0= 0< = < >

: =      ( a b -- flag )     - 0= ;
: <      ( a b -- flag )     - 0< ;
: >      ( a b -- flag )     swap < ;

: boot
  0 0= 70 + emit            \ E
  1 0= 70 + emit            \ F
  -1 0< 84 + emit           \ S
  1 0< 84 + emit            \ T
  5 5 = 70 + emit           \ E
  3 5 < 84 + emit           \ S
  7 2 > 84 + emit           \ S
  bye
;
