(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE Pointer;

TYPE
  IntPtr = POINTER TO INTEGER;
  CardPtr = POINTER TO CARDINAL;
  PointPtr = POINTER TO Point;
  Point = RECORD
            x, y: CARDINAL
          END;

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

PROCEDURE InitPoint():PointPtr;
BEGIN
  RETURN NIL;
END InitPoint;
(*
CHECK-LABEL: _m7Pointer9InitPoint
CHECK: ret ptr null
*)

END Pointer.
