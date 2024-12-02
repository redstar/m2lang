(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
(* See https://www.arjay.bc.ca/Modula-2/Text/index.html?https://www.arjay.bc.ca/Modula-2/Text/Ch10/Ch10.15.html *)
MODULE CheckModVisibility2;

MODULE Inside1;
EXPORT number1;
VAR
  number1 : REAL;

BEGIN  (* Only number1 is visible here *)
  number1 := 5.4; (* number1 can be initialized here *)
END Inside1;

MODULE Inside2;
IMPORT Inside1;
VAR
  number2 : REAL;

BEGIN   (* number2, number1 both visible here. *)
  number1 := 3.4;
  Inside1.number1 := 5.7;
  number2 := 3.9
END Inside2;

BEGIN (* Test *)
  (* only number1 visible here *)
END CheckModVisibility2.