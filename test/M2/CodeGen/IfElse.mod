(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE IfElse;

PROCEDURE Test1(x: INTEGER):INTEGER;
BEGIN
  IF x >= 10 THEN
    RETURN 10;
  ELSIF x = 9 THEN
    RETURN 9;
  ELSIF x = 5 THEN
    RETURN 5;
  ELSE
    RETURN 0;
  END;
END Test1;
(*
CHECK-LABEL: _m6IfElse5Test1
*)

PROCEDURE Test2(x: INTEGER):INTEGER;
BEGIN
  IF x >= 10 THEN
    RETURN 10;
  ELSIF x = 9 THEN
    RETURN 9;
  ELSIF x = 5 THEN
    RETURN 5;
  END;
  RETURN 0;
END Test2;
(*
CHECK-LABEL: _m6IfElse5Test2
*)

END IfElse.
