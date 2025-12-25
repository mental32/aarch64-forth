\ Standard-ish core Forth words extracted from the kernel Forth sources.

: over   ( a b -- a b a )    >r dup r> swap ;
: nip    ( a b -- b )        swap drop ;
: tuck   ( a b -- b a b )    swap over ;
: rot    ( a b c -- b c a )  >r swap r> swap ;
: -rot   ( a b c -- c a b )  swap >r swap r> ;
: 2dup   ( a b -- a b a b )  over over ;
: 2drop  ( a b -- )          drop drop ;
: rdrop  ( -- )              r> drop ;

: negate ( n -- -n )         0 swap - ;
: 1+     ( n -- n+1 )        1 + ;
: 1-     ( n -- n-1 )        1 - ;
: 2*     ( n -- 2*n )        dup + ;

: +!     ( n addr -- )       dup @ rot + swap ! ;

: =      ( a b -- flag )     - 0= ;
: <      ( a b -- flag )     - 0< ;
: >      ( a b -- flag )     swap < ;

: here   (  -- addr )
  &dict_here @ ;
: allot  (  n -- )
  &dict_here @ + &dict_here ! ;
: ,      (  x -- )
  &dict_here @
  dup 8 + &dict_here !
  ! ;
: c,     (  c -- )
  &dict_here @
  dup 1 + &dict_here !
  c! ;

: align-here  (  -- )
  &dict_here @
  dup 7 + -8 and
  &dict_here !
  drop ;

: immediate  (  -- )
  \ Mark the most recently defined word as IMMEDIATE by
  \ setting the low bit in its header flags byte.
  &latest @ 8 +        \ addr of flags byte
  dup c@ 1 or          \ set bit 0
  swap c! ;

: if     (  -- orig )
  &xt_forth_zero_branch ,   \ compile 0branch
  here                 \ placeholder address
  0 ,                  \ reserve slot
; immediate

: then   (  orig -- )
  here swap ! ; immediate

: else   (  orig -- orig2 )
  &xt_forth_branch ,         \ compile jump over ELSE
  here                 \ new placeholder for AFTER ELSE
  0 ,                  \ reserve slot
  swap                 \ bring old orig on top
  here swap !          \ patch old orig to ELSE start
; immediate

: begin  (  -- dest )
  here ; immediate

: again  (  dest -- )
  &xt_forth_branch , ,
; immediate

: until  (  dest -- )
  &xt_forth_zero_branch , ,
; immediate

: while  (  dest -- dest orig )
  &xt_forth_zero_branch ,    \ compile conditional branch
  here                 \ placeholder addr
  0 ,                  \ reserve slot
  swap                 \ keep loop start underneath
; immediate

: repeat (  dest orig -- )
  &xt_forth_branch , ,       \ jump back to begin
  here swap !          \ patch WHILE placeholder to here
; immediate

: cr      13 emit 10 emit ;
: space   32 emit ;
: spaces  (  n -- )
  dup 0= IF drop EXIT THEN
  space 1- spaces ;

: 2@  (  addr -- x1 x2 )
  dup @ swap 8 + @ ;

: 2!  (  x1 x2 addr -- )
  >r swap r@ ! r> 8 + ! ;

: 0<> (  n -- flag )
  0= invert ;

: 0>  (  n -- flag )
  0 swap < ;

: TRUE  -1 ;
: FALSE 0 ;

: <=  (  a b -- flag )
  2dup < >r = r> or ;

: >=  (  a b -- flag )
  2dup > >r = r> or ;

: u<  (  u1 u2 -- flag )
  2dup xor 0< IF
    swap drop 0<
  ELSE
    - 0<
  THEN ;

: /mod  (  n d -- rem quot )
  2dup / >r
  mod
  r> ;

\ Defining words
: start-header  ( addr len -- hdr )
  here >r                     \ remember header start
  &latest @ ,                 \ link field -> previous latest
  0 c,                        \ flags byte
  dup c,                      \ name length byte
  \ copy name characters
  begin
    dup 0<> WHILE
      over c@ c,
      swap 1+ swap
      1-
    REPEAT
  drop drop
  align-here
  r> ;

: create  ( "name" -- )
  next-token IF               ( addr len )
    start-header
    dup &latest ! drop
    &forth_dodoes ,
    0 ,
  ELSE
    drop drop
  THEN ;

: constant  ( n "name" -- )
  create ,
  does> @ ;
