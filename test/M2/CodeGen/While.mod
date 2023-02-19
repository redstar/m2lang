(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE While;

PROCEDURE Test1(a, b: INTEGER):INTEGER;
VAR t: INTEGER;
BEGIN
  IF b = 0 THEN
    RETURN a;
  END;
  WHILE b # 0 DO
    t := a MOD b;
    a := b;
    b := t;
  END;
  RETURN a;
END Test1;
(*
CHECK-LABEL: _m5While5Test1
CHECK: %1 = phi i64 [ %2, %while.body ], [ %a, %entry ]
CHECK: %2 = phi i64 [ %4, %while.body ], [ %b, %entry ]
*)

PROCEDURE Test2(VAR a: INTEGER; b: INTEGER);
VAR t: INTEGER;
BEGIN
  IF b = 0 THEN
    RETURN;
  END;
  WHILE b # 0 DO
    t := a MOD b;
    a := b;
    b := t;
  END;
END Test2;
(*
CHECK-LABEL: _m5While5Test2
CHECK: %1 = phi i64 [ %4, %while.body ], [ %b, %entry ]
CHECK: %3 = load i64, ptr %a, align 8
CHECK: store i64 %1, ptr %a, align 8
*)

END While.
