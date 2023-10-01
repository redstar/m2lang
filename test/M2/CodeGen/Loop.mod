(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
TODO The generated IR is wrong!
*)
MODULE Loop;

PROCEDURE Test1(num: INTEGER):INTEGER;
VAR sum: INTEGER;
BEGIN
  sum := 0;
  LOOP
    sum := sum + num;
    num := num - 2;
    IF num <= 0 THEN EXIT END;
  END;
  RETURN sum;
END Test1;
(*
CHECK-LABEL: _m4Loop5Test1
*)

END Loop.
