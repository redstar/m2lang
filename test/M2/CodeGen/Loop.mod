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
CHECK-LABEL: _m4Loop5Test1
CHECK:      loop.body:
CHECK-NEXT: %0 = phi i64 [ %3, %after.if ], [ %num, %entry ]
CHECK-NEXT: %1 = phi i64 [ %2, %after.if ], [ 0, %entry ]
*)

END Loop.
