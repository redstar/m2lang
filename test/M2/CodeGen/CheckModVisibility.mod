(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
(* See https://www.arjay.bc.ca/Modula-2/Text/index.html?https://www.arjay.bc.ca/Modula-2/Text/Ch10/Ch10.15.html *)
MODULE CheckModVisibility;

MODULE Inner;
EXPORT number;
VAR
  number : REAL;

END Inner;

BEGIN
  number := 5.0;
  Inner.number := 9.8;  (* both references legal *)
END CheckModVisibility.