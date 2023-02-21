(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
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
CHECK-LABEL: _m6Repeat5Test1
CHECK: %0 = phi i64 [ %3, %repeat.body ], [ %num, %entry ]
CHECK: %1 = phi i64 [ %2, %repeat.body ], [ 0, %entry ]
*)

END Loop.
