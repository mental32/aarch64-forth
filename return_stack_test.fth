\ EXPECT: MM
\ Return stack: >r r@ r>

: boot
  77 >r          \ push 'M'
  r@ emit        \ M
  r> emit        \ M
  bye
;
