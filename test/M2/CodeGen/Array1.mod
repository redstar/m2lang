(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE Array1;

TYPE
  Color = (Red, Green, Blue, Cyan, Yellow, White, Purple, Black);

VAR
  ColorWeight : ARRAY Color OF CARDINAL;

PROCEDURE SetValue(Index: Color; Weight: CARDINAL);
BEGIN
  ColorWeight[Index] := Weight;
END SetValue;
(* IR is wrong. *)

PROCEDURE GetValue(Index: Color; VAR Weight: CARDINAL);
BEGIN
  Weight := ColorWeight[Index];
END GetValue;
(* IR is correct, assuming a 64 bit platform. *)
(*
CHECK: define void @_m6Array18GetValue(i64 %Index, ptr nocapture dereferenceable(8) %Weight) {
CHECK: entry:
CHECK:   %0 = getelementptr inbounds ptr, ptr @_m6Array111ColorWeight, i64 %Index
CHECK:   %1 = load i64, ptr %0, align 8
CHECK:   store i64 %1, ptr %Weight, align 8
CHECK: }
*)

END Array1.
