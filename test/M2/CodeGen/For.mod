(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE For;

PROCEDURE Test1():INTEGER;
VAR x, y: INTEGER;
BEGIN
  y := 0;
  FOR x := 1 TO 10 DO
    y := y + 2;
  END;
  RETURN y;
END Test1;
(*
CHECK-LABEL: _m3For5Test1
CHECK: %0 = phi i64 [ %3, %for.body ], [ 0, %entry ]
CHECK: %1 = phi i64 [ %4, %for.body ], [ 1, %entry ]
*)

PROCEDURE Test2():INTEGER;
VAR x, y: INTEGER;
BEGIN
  y := 0;
  FOR x := 1 TO 10 BY 2 DO
    y := y + 2;
  END;
  RETURN y;
END Test2;
(*
CHECK-LABEL: _m3For5Test2
CHECK: %0 = phi i64 [ %3, %for.body ], [ 0, %entry ]
CHECK: %1 = phi i64 [ %4, %for.body ], [ 1, %entry ]
*)

END For.
