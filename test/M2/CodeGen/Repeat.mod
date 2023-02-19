(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE Repeat;

PROCEDURE Test1(num: INTEGER):INTEGER;
VAR sum: INTEGER;
BEGIN
  sum := 0;
  REPEAT
    sum := sum + num;
    num := num - 2;
  UNTIL num = 0;
  RETURN sum;
END Test1;
(*
CHECK-LABEL: _m6Repeat5Test1
CHECK: %0 = phi i64 [ %3, %repeat.body ], [ %num, %entry ]
CHECK: %1 = phi i64 [ %2, %repeat.body ], [ 0, %entry ]
*)

END Repeat.
