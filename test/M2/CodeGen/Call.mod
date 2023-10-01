(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
Still crashes because function calls are not yet implemented!
XFAIL: *
*)
MODULE Call;

PROCEDURE Get():INTEGER;
BEGIN
  RETURN 5;
END Get;

PROCEDURE Test1():INTEGER;
BEGIN
  IF Get() > 0 THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END;
END Test1;
(*
CHECK-LABEL:
CHECK:
*)

END Call.
