(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
*)
(* See https://www.arjay.bc.ca/Modula-2/Text/index.html?https://www.arjay.bc.ca/Modula-2/Text/Ch10/Ch10.15.html *)
MODULE CheckModVisibility3;

  MODULE Shell1;
  EXPORT
    Inner1;

    MODULE Inner1;
    EXPORT thing;
    VAR
      thing: CARDINAL;
    END Inner1;

  END Shell1;

  MODULE Shell2;
  EXPORT Inner2;

    MODULE Inner2;
    EXPORT QUALIFIED thing;
    VAR
      thing: CARDINAL;
    END Inner2;

  END Shell2;

  MODULE Shell3;
  EXPORT QUALIFIED Inner3;

    MODULE Inner3;
    EXPORT QUALIFIED thing;
    VAR
      thing: CARDINAL;
    END Inner3;

  END Shell3;

(* here in this outer one, Inner1 is visible, so its exports may be qualified *)
(* Inner2 is visible, so its qualified exports may be qualified *)
(* however, Inner3 itself must be qualified *)
BEGIN
  Inner1.thing := 5;
  Inner2.thing := 4;
  Shell3.Inner3.thing := 8;
END CheckModVisibility3.