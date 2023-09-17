(*
COM: RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
Selectors for records are wrong!
*)
MODULE PassByRef;

PROCEDURE SetInt(VAR i: INTEGER);
BEGIN
  i := 0;
END SetInt;
(*
CHECK-LABEL:
CHECK:
*)

TYPE
  Person = RECORD
             Height, Weight, Age: INTEGER;
           END;

PROCEDURE SetPerson(VAR p: Person);
BEGIN
  p.Height := 180;
  p.Weight := 80;
  p.Age := 18;
END SetPerson;
(*
CHECK-LABEL:
CHECK:
*)

END PassByRef.
