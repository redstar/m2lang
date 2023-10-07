(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
MODULE LocalMod;

MODULE A;
EXPORT Color, AColor;
TYPE
  Color = (Yellow, Red, Blue, Purple);
VAR
  AColor: Color;
END A;
(*
CHECK: @_m8LocalMod1A6AColor = private global i64
*)

VAR
  TheColor: Color;
(*
CHECK: @_m8LocalMod8TheColor = private global i64
*)

PROCEDURE InitColor;
BEGIN
  TheColor := Yellow;
  AColor := Purple;
END InitColor;
(*
CHECK-LABEL: _m8LocalMod9InitColor
CHECK:       store i64 0, ptr @_m8LocalMod8TheColor, align 8
CHECK:       store i64 3, ptr @_m8LocalMod1A6AColor, align 8
*)

(* Imports are not yet working.
MODULE B;
IMPORT Color;
EXPORT CurrentColor;
VAR
  CurrentColor: Color;
END B;

MODULE C;
IMPORT Color, CurrentColor;
EXPORT SetColor;
PROCEDURE SetColor(C: Color);
BEGIN
  (* Replace C with Color to produce crash in gm2. *)
  CurrentColor := C;
END SetColor;
END C;
*)

END LocalMod.
