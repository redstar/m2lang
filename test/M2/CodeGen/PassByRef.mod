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
CHECK-LABEL: _m9PassByRef6SetInt
CHECK: store i64 0, ptr %i, align 8
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
CHECK-LABEL: _m9PassByRef9SetPerson
TODO Selectors are wrong!
*)

END PassByRef.
