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
CHECK: define i64* @_m7Pointer7InitInt()
CHECK: null
*)

PROCEDURE InitCard():CardPtr;
BEGIN
  RETURN NIL;
END InitCard;
(*
CHECK: define i64* @_m7Pointer8InitCard()
CHECK: null
*)

END Pointer.
