(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
XFAIL: *
*)
MODULE OpenArray;

(* Arrays should be passed as length + pointer to data.
   E.g. %array = type { i32, ptr }
*)

(* How are arrays passed? Pointer to local copy? *)
PROCEDURE Test1(x: ARRAY OF CHAR):CHAR;
BEGIN
  RETURN x[1];
END Test1;

(* CodeGen is completely broken for this case. *)
(* Expected IR (bounds check missing):

define signext i8 @_m9OpenArray5Test2(i32 %len, ptr %data) {
  %ptr = getelementptr inbounds i8, ptr %data, i64 1
  %res = load i8, ptr %ptr, align 1
  ret i8 %res
}

*)
PROCEDURE Test2(VAR x: ARRAY OF CHAR):CHAR;
BEGIN
  RETURN x[1];
END Test2;

END OpenArray.
