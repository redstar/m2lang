(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE Pointer;

TYPE
  IntPtr = POINTER TO INTEGER;
  CardPtr = POINTER TO CARDINAL;

PROCEDURE InitInt():IntPtr;
BEGIN
  RETURN NIL;
END InitInt;
(*
CHECK-LABEL: _m7Pointer7InitInt
CHECK: ret ptr null
*)

PROCEDURE InitCard():CardPtr;
BEGIN
  RETURN NIL;
END InitCard;
(*
CHECK-LABEL: _m7Pointer8InitCard
CHECK: ret ptr null
*)

END Pointer.
