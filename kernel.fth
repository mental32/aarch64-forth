\ Forth implementations for words moved out of kernel.S
include prelude.fth

: digit>char  ( u -- c )
  9 > IF 55 + ELSE 48 + THEN ;

: .hex-byte  ( u -- )
  dup 15 and >r        \ save low nibble
  16 / 15 and          \ high nibble
  digit>char emit
  r> digit>char emit ;

: .hex  ( u -- )
  dup 16 /mod swap
  digit>char emit
  digit>char emit ;

: .hex32  ( u -- )
  dup 256 /mod >r            \ save low byte, keep high part
  dup 256 /mod >r            \ save next byte
  dup 256 /mod >r            \ save third byte
  .hex-byte                  \ emit highest byte (remaining on stack)
  r> .hex-byte               \ third
  r> .hex-byte               \ second
  r> .hex-byte ;             \ low

: hex64  ( u -- )
  dup 256 /mod >r
  dup 256 /mod >r
  dup 256 /mod >r
  dup 256 /mod >r
  dup 256 /mod >r
  dup 256 /mod >r
  dup 256 /mod >r
  .hex-byte
  r> .hex-byte
  r> .hex-byte
  r> .hex-byte
  r> .hex-byte
  r> .hex-byte
  r> .hex-byte
  r> .hex-byte ;

: move  ( src dest count -- )
  begin
    dup 0<> while
      >r
      >r
      dup c@
      r@ c!
      1+
      r> 1+
      r> 1-
    repeat
  drop drop drop ;

: fill  ( addr count char -- )
  >r
  begin
    dup 0<> while
      over r@ c!
      swap 1+ swap
      1-
    repeat
  drop drop rdrop ;

: abs  ( n -- u )
  dup 0< IF negate THEN ;

: min  ( a b -- n )
  2dup < IF drop ELSE nip THEN ;

: max  ( a b -- n )
  2dup > IF drop ELSE nip THEN ;

\ Merged interpreter and compiler definitions
include read-line.fth

: init-dict  ( -- )
  &bss_end &dict_here !
  0 &state ! ;

: prompt  79 emit 75 emit 62 emit 32 emit ;  \ emit "ok> " directly

: dump-dict  ( -- )
  &latest @
  begin
    dup 0<> while
      dup 8 + c@ emit          \ emit name length for trace
      dup 10 + c@ emit         \ emit first char of name
      dup @                    \ follow link
    repeat
  drop ;

\ Token classification helpers
: char-token=  ( addr len ch -- flag )
  >r
  dup 1 = IF
    drop
    c@ r> =
  ELSE
    drop drop
    r> drop
    FALSE
  THEN ;

: is-colon-token      ( addr len -- flag )
  58 char-token= ;
: is-semicolon-token  ( addr len -- flag )
  59 char-token= ;

\ Core interpreter/compiler actions
: interpret-token  ( addr len -- )
  \ Use find-name's flag semantics:
  \   0  -> not found
  \  -1  -> found, normal word
  \   1  -> found, immediate word
  2dup find-name              \ addr len xt flag
  dup IF                      \ non-zero => found
    drop                      \ drop flag, keep xt
    >r 2drop r> execute
  ELSE
    drop                      \ drop flag
    drop                      \ drop xt
    \ try to parse as number
    2dup parse-num IF
      >r drop drop r>
    ELSE
      drop drop drop
      ." ?"
    THEN
  THEN ;

: compile-token  ( addr len -- )
  2dup find-name              \ addr len xt flag
  dup IF                      \ non-zero => found
    >r                        \ R: flag
    >r                        \ R: xt flag
    drop drop                 \ drop addr len
    r>                        \ xt
    r>                        \ flag
    0< IF                     \ normal word: compile XT
      ,
    ELSE                      \ immediate word: execute at compile time
      execute
    THEN
  ELSE
    drop                      \ flag
    drop                      \ xt
    drop drop                 \ addr len
    ." ?"
  THEN ;

: start-colon  ( -- )
  next-token IF               \ addr len
    start-header              \ hdr
    dup &latest ! drop        \ latest = hdr
    &forth_docol ,                  \ CFA: colon entry code
    TRUE &state !
  ELSE
    drop drop
    ." : name missing" cr
  THEN ;

: end-colon  ( -- )
  &xt_forth_exit ,                  \ ensure colon definition returns
  0 &state ! ;

: handle-token  ( addr len -- )
  2dup is-colon-token IF
    drop drop
    &state @ 0= IF
      start-colon
    ELSE
      ." nested : not allowed" cr
    THEN
  ELSE
    2dup is-semicolon-token IF
      drop drop
      &state @ 0<> IF
        end-colon
      ELSE
        ." ; without matching :" cr
      THEN
    ELSE
      &state @ 0= IF
        interpret-token
      ELSE
        compile-token
      THEN
    THEN
  THEN ;

: interpret  ( -- )
  ;

: boot  ( -- )
  init-dict
  bye ;
