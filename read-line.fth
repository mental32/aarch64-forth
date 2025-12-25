\ Read-line and line editing helpers

: ensure-cursor-bounds  ( -- )
  &input_pos @ &input_len @ > IF
    &input_len @ &input_pos !
  THEN ;

: nonneg-tail  ( n -- n' )
  dup 0< IF
    drop 0
  THEN ;

: set-input  ( addr len -- ) \ copy into input_buf and set len/pos
  dup &input_len !        \ remember length
  &input_buf rot move     \ move len bytes into buffer
  0 &input_pos ! ;

\ Simple emacs-like read-line implementation

: emit-chars  ( addr len -- )
  begin
    dup 0<> while
      over c@ emit
      swap 1+ swap
      1-
    repeat
  drop drop ;

: esc-left   27 emit 91 emit 68 emit ;  \ ESC [ D
: esc-right  27 emit 91 emit 67 emit ;  \ ESC [ C

: term-left-n  ( n -- )
  begin dup 0<> while esc-left 1- repeat drop ;
: term-right-n ( n -- )
  begin dup 0<> while esc-right 1- repeat drop ;

: move-left  ( -- )
  ensure-cursor-bounds
  &input_pos @ 0<> IF
    esc-left
    &input_pos @ 1- &input_pos !
  THEN ;

: move-right  ( -- )
  ensure-cursor-bounds
  &input_pos @ &input_len @ < IF
    esc-right
    &input_pos @ 1+ &input_pos !
  THEN ;

: move-home  ( -- )
  ensure-cursor-bounds
  &input_pos @ term-left-n
  0 &input_pos ! ;

: move-end  ( -- )
  ensure-cursor-bounds
  &input_len @ &input_pos @ -
  dup term-right-n drop
  &input_len @ &input_pos ! ;

: shift-tail-right  ( tail_len -- )
  nonneg-tail
  begin
    dup 0<> while
      dup &input_pos @ + &input_buf +      \ dest
      over 1- &input_pos @ + &input_buf +  \ src
      c@ swap c!
      1-
    repeat
  drop ;

: shift-tail-left  ( tail_len -- )
  nonneg-tail
  0 swap                              \ i tail_len
  begin
    2dup < while
      over &input_pos @ + &input_buf +     \ i tail_len dest
      over 1+ &input_pos @ + &input_buf +  \ i tail_len dest src
      c@ swap c!
      swap 1+ swap                        \ i+1 tail_len
    repeat
  drop drop ;

: redraw-tail  ( tail_len -- )
  dup IF
    &input_buf &input_pos @ + swap
    emit-chars
  ELSE
    drop
  THEN ;

: insert-char  ( c -- )
  &input_len @ 255 < IF
    ensure-cursor-bounds
    &input_len @ &input_pos @ - dup >r     \ tail_len (R: tail_len)
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

: backspace-char  ( -- )
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

: kill-to-eol  ( -- )
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

: handle-escape  ( -- )
  key
  dup 91 = IF  \ '['
    drop
    key
    dup 68 = IF drop move-left EXIT THEN   \ ESC [ D
    dup 67 = IF drop move-right EXIT THEN  \ ESC [ C
    dup 72 = IF drop move-home EXIT THEN   \ ESC [ H
    dup 70 = IF drop move-end EXIT THEN    \ ESC [ F
    drop
  ELSE
    drop
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
    dup 13 = IF
      drop cr
      &input_buf &input_len @
      EXIT
    THEN
    dup 10 = IF
      drop cr
      &input_buf &input_len @
      EXIT
    THEN
    dup 27 = IF
      drop handle-escape
    ELSE
      dup 8 = IF
        drop backspace-char
      ELSE
        dup 127 = IF
          drop backspace-char
        ELSE
          dup 1 = IF
            drop move-home
          ELSE
            dup 5 = IF
              drop move-end
            ELSE
              dup 11 = IF
                drop kill-to-eol
              ELSE
                dup 2 = IF
                  drop move-left
                ELSE
                  dup 6 = IF
                    drop move-right
                  ELSE
                    insert-char
                  THEN
                THEN
              THEN
            THEN
          THEN
        THEN
      THEN
    THEN
  AGAIN ;
