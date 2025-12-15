\ Core primitives implemented in Forth
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

: here   \ ( -- addr )
  &dict_here @ ;
: allot  \ ( n -- )
  &dict_here @ + &dict_here ! ;
: ,      \ ( x -- )
  &dict_here @
  dup 8 + &dict_here !
  ! ;
: c,     \ ( c -- )
  &dict_here @
  dup 1 + &dict_here !
  c! ;

: align-here  \ ( -- )
  &dict_here @
  dup 7 + -8 and
  &dict_here !
  drop ;

: immediate  \ ( -- )
  \ Mark the most recently defined word as IMMEDIATE by
  \ setting the low bit in its header flags byte.
  &latest @ 8 +        \ addr of flags byte
  dup c@ 1 or          \ set bit 0
  swap c! ;


: init-dict  \ ( -- )
  &bss_end &dict_here !
  0 &state ! ;

: if     \ ( -- orig )
  &xt_forth_zero_branch ,   \ compile 0branch
  here                 \ placeholder address
  0 ,                  \ reserve slot
; immediate

: then   \ ( orig -- )
  here swap ! ; immediate

: else   \ ( orig -- orig2 )
  &xt_forth_branch ,         \ compile jump over ELSE
  here                 \ new placeholder for AFTER ELSE
  0 ,                  \ reserve slot
  swap                 \ bring old orig on top
  here swap !          \ patch old orig to ELSE start
; immediate

: begin  \ ( -- dest )
  here ; immediate

: again  \ ( dest -- )
  &xt_forth_branch , ,
; immediate

: until  \ ( dest -- )
  &xt_forth_zero_branch , ,
; immediate

: while  \ ( dest -- dest orig )
  &xt_forth_zero_branch ,    \ compile conditional branch
  here                 \ placeholder addr
  0 ,                  \ reserve slot
  swap                 \ keep loop start underneath
; immediate

: repeat \ ( dest orig -- )
  &xt_forth_branch , ,       \ jump back to begin
  here swap !          \ patch WHILE placeholder to here
; immediate

: cr      13 emit 10 emit ;
: space   32 emit ;



\ Words that use control flow (moved here after IF/THEN are defined)
: spaces  \ ( n -- )
  dup 0= IF drop EXIT THEN
  space 1- spaces ;

: 2@  \ ( addr -- x1 x2 )
  dup @ swap 8 + @ ;

: 2!  \ ( x1 x2 addr -- )
  >r swap r@ ! r> 8 + ! ;

: 0<> \ ( n -- flag )
  0= invert ;

: 0>  \ ( n -- flag )
  0 swap < ;

: TRUE  -1 ;
: FALSE 0 ;

: ensure-cursor-bounds  \ ( -- )
  &input_pos @ &input_len @ > IF
    &input_len @ &input_pos !
  THEN ;

: nonneg-tail  \ ( n -- n' )
  dup 0< IF
    ." [DBG] negative tail clamped" cr
    drop 0
  THEN ;

: <=  \ ( a b -- flag )
  2dup < >r = r> or ;

: >=  \ ( a b -- flag )
  2dup > >r = r> or ;

: u<  \ ( u1 u2 -- flag )
  2dup xor 0< IF
    swap drop 0<
  ELSE
    - 0<
  THEN ;

: /mod  \ ( n d -- rem quot )
  2dup / >r
  mod
  r> ;

: digit>char  \ ( u -- c )
  9 > IF 55 + ELSE 48 + THEN ;

: .hex-byte  \ ( u -- )
  dup 15 and >r        \ save low nibble
  16 / 15 and          \ high nibble
  digit>char emit
  r> digit>char emit ;

: .hex  \ ( u -- )
  dup 16 /mod swap
  digit>char emit
  digit>char emit ;

: .hex32  \ ( u -- )
  dup 256 /mod >r            \ save low byte, keep high part
  dup 256 /mod >r            \ save next byte
  dup 256 /mod >r            \ save third byte
  .hex-byte                  \ emit highest byte (remaining on stack)
  r> .hex-byte               \ third
  r> .hex-byte               \ second
  r> .hex-byte ;             \ low

: dbg-key  \ ( c -- )
  dup >r
  ." [DBG-key " .hex-byte
  ." dec=" r@ .
  ." ]" cr
  rdrop ;

: set-input  \ ( addr len -- ) copy into input_buf and set len/pos
  dup &input_len !        \ remember length
  &input_buf rot move     \ move len bytes into buffer
  0 &input_pos ! ;

: load-test-input  \ ( -- ) preload \"123\" into input buffer
  0 &input_pos !
  3 &input_len !
  49 &input_buf c!        \ '1'
  50 &input_buf 1 + c!    \ '2'
  51 &input_buf 2 + c! ;  \ '3'

\ Read a bounded number of raw UART bytes and log them (hex + dec).
: uart-probe  \ ( count -- )
  begin
    dup 0<> while
      key dup dbg-key drop
      1-
    repeat
  drop ;

\ Simple emacs-like read-line implementation

: emit-chars  \ ( addr len -- )
  begin
    dup 0<> while
      over c@ emit
      swap 1+ swap
      1-
    repeat
  drop drop ;

: esc-left   27 emit 91 emit 68 emit ;  \ ESC [ D
: esc-right  27 emit 91 emit 67 emit ;  \ ESC [ C

: term-left-n  \ ( n -- )
  begin dup 0<> while esc-left 1- repeat drop ;
: term-right-n \ ( n -- )
  begin dup 0<> while esc-right 1- repeat drop ;

: move-left  \ ( -- )
  ensure-cursor-bounds
  &input_pos @ 0<> IF
    esc-left
    &input_pos @ 1- &input_pos !
  THEN ;

: move-right  \ ( -- )
  ensure-cursor-bounds
  &input_pos @ &input_len @ < IF
    esc-right
    &input_pos @ 1+ &input_pos !
  THEN ;

: move-home  \ ( -- )
  ensure-cursor-bounds
  &input_pos @ term-left-n
  0 &input_pos ! ;

: move-end  \ ( -- )
  ensure-cursor-bounds
  &input_len @ &input_pos @ -
  dup term-right-n drop
  &input_len @ &input_pos ! ;

: shift-tail-right  \ ( tail_len -- )
  nonneg-tail
  ." [DBG] shift-right tail=" dup .hex cr
  begin
    dup 0<> while
      dup &input_pos @ + &input_buf +      \ dest
      over 1- &input_pos @ + &input_buf +  \ src
      c@ swap c!
      1-
    repeat
  drop ;

: shift-tail-left  \ ( tail_len -- )
  nonneg-tail
  ." [DBG] shift-left tail=" dup .hex cr
  0 swap                              \ i tail_len
  begin
    2dup < while
      over &input_pos @ + &input_buf +     \ i tail_len dest
      over 1+ &input_pos @ + &input_buf +  \ i tail_len dest src
      c@ swap c!
      swap 1+ swap                        \ i+1 tail_len
    repeat
  drop drop ;

: redraw-tail  \ ( tail_len -- )
  dup IF
    &input_buf &input_pos @ + swap
    emit-chars
  ELSE
    drop
  THEN ;

: insert-char  \ ( c -- )
  &input_len @ 255 < IF
    ensure-cursor-bounds
    &input_len @ &input_pos @ - dup >r     \ tail_len (R: tail_len)
    ." [DBG] insert tail=" r@ .hex ." pos=" &input_pos @ .hex ." len=" &input_len @ .hex cr
    shift-tail-right                       \ make room
    dup emit                               \ echo char
    &input_buf &input_pos @ + c!           \ store at cursor
    &input_len @ 1+ &input_len !
    &input_pos @ 1+ &input_pos !
    r@ redraw-tail
    r@ term-left-n
    rdrop
  ELSE
    drop
  THEN ;

: backspace-char  \ ( -- )
  ensure-cursor-bounds
  &input_pos @ 0= IF EXIT THEN
  &input_pos @ 1- &input_pos !
  &input_len @ &input_pos @ - 1- dup >r    \ tail_len (R: tail_len)
  shift-tail-left
  &input_len @ 1- &input_len !
  esc-left
  r@ redraw-tail
  space
  r@ 1+ term-left-n
  rdrop ;

: kill-to-eol  \ ( -- )
  ensure-cursor-bounds
  &input_len @ &input_pos @ - dup nonneg-tail dup >r       \ tail_len (R: tail_len)
  &input_pos @ &input_len !
  r@ dup 0<> IF
    begin dup 0<> while space 1- repeat drop
    r@ term-left-n
  ELSE
    drop
  THEN
  rdrop ;

: handle-escape  \ ( -- )
  key
  dup 91 = IF  \ '['
    drop
    key
    dup 68 = IF drop move-left EXIT THEN   \ ESC [ D
    dup 67 = IF drop move-right EXIT THEN  \ ESC [ C
    dup 72 = IF drop move-home EXIT THEN   \ ESC [ H
    dup 70 = IF drop move-end EXIT THEN    \ ESC [ F
    ." [DBG] unknown CSI" cr drop
  ELSE
    ." [DBG] lone ESC" cr drop
  THEN ;

\ Compatibility shim for tests that use read-line-core
: read-line-core  ( -- addr len )
  &input_buf &input_len @ ;

: read-line  ( -- addr len )
  \ If buffer already has content, return it (test compatibility)
  &input_len @ dup 0<> IF
    drop
    0 &input_pos !
    &input_buf &input_len @
    EXIT
  THEN

\ Initialize for new input
  0 &input_len !
  0 &input_pos !

  BEGIN
    key 255 and
    dup dbg-key
    ." [DBG] key '" dup emit ." '" cr
    ." [DBG] len=" &input_len @ .hex-byte ." pos=" &input_pos @ .hex-byte cr
    dup 13 = IF
      ." [DBG] CR" cr
      drop cr
      &input_buf &input_len @
      EXIT
    THEN
    dup 10 = IF
      ." [DBG] LF" cr
      drop cr
      &input_buf &input_len @
      EXIT
    THEN
    dup 27 = IF
      ." [DBG] ESC" cr
      drop handle-escape
    ELSE
      dup 8 = IF
        ." [DBG] BS" cr
        drop backspace-char
      ELSE
        dup 127 = IF
          ." [DBG] DEL" cr
          drop backspace-char
        ELSE
          dup 1 = IF
            ." [DBG] ^A" cr
            drop move-home
          ELSE
            dup 5 = IF
              ." [DBG] ^E" cr
              drop move-end
            ELSE
              dup 11 = IF
                ." [DBG] ^K" cr
                drop kill-to-eol
              ELSE
                dup 2 = IF
                  ." [DBG] ^B" cr
                  drop move-left
                ELSE
                  dup 6 = IF
                    ." [DBG] ^F" cr
                    drop move-right
                  ELSE
                    ." [DBG] insert" cr
                    drop insert-char
                  THEN
                THEN
              THEN
            THEN
          THEN
        THEN
      THEN
    THEN
    ." [DBG] end-iter len=" &input_len @ .hex-byte ." pos=" &input_pos @ .hex-byte cr
  AGAIN ;

: prompt  79 emit 75 emit 62 emit 32 emit ;  \ emit "ok> " directly

: dump-dict  \ ( -- )
  &latest @
  begin
    dup 0<> while
      dup 8 + c@ emit          \ emit name length for trace
      dup 10 + c@ emit         \ emit first char of name
      dup @                    \ follow link
    repeat
  drop ;

\ Token classification helpers
: char-token=  \ ( addr len ch -- flag )
  >r
  dup 1 = IF
    drop
    c@ r> =
  ELSE
    drop drop
    r> drop
    FALSE
  THEN ;

: is-colon-token      \ ( addr len -- flag )
  58 char-token= ;
: is-semicolon-token  \ ( addr len -- flag )
  59 char-token= ;

\ Core interpreter/compiler actions
: interpret-token  \ ( addr len -- )
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

: compile-token  \ ( addr len -- )
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

: start-header  \ ( addr len -- hdr )
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

: start-colon  \ ( -- )
  next-token IF               \ addr len
    start-header              \ hdr
    dup &latest ! drop        \ latest = hdr
    &forth_docol ,                  \ CFA: colon entry code
    TRUE &state !
  ELSE
    drop drop
    ." : name missing" cr
  THEN ;

: end-colon  \ ( -- )
  &xt_forth_exit ,                  \ ensure colon definition returns
  0 &state ! ;

: handle-token  \ ( addr len -- )
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

: interpret  \ ( -- )
  ." [DBG] interpreter disabled for UART probing" cr ;

: boot  \ ( -- )
  init-dict
  ." [BOOT] UART sanity" cr
  ." shadow=" uart-base hex64 space ." reg=" uart-base-reg hex64 cr
  ." FR0=" uart-fr hex64 cr
  ." IBRD=" uart-ibrd hex64 space
  ." FBRD=" uart-fbrd hex64 space
  ." LCRH=" uart-lcrh hex64 space
  ." CR=" uart-cr hex64 cr
  ." [BOOT] waiting for 1 UART byte (poll)" cr
  ." [MARK] before poll" cr
  uart-poll dup ." [DBG] polled byte=" hex64 cr drop
  ." [MARK] after poll" cr
  ." [BOOT] after poll FR=" uart-fr hex64 space ." DR=" uart-dr hex64 cr
  cr ." [BOOT] halt" cr
  begin again ;
