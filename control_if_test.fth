\ EXPECT: A
\ IF / ELSE / THEN
include prelude.fth
: boot
  -1 IF
    65 emit                  \ A
  ELSE
    66 emit                  \ B
  THEN

  0 IF
    67 emit                  (skipped)
  THEN
  bye
;
