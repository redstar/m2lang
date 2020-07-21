(*
RUN: m2lang -DBRANCH=b1 -filetype=asm -emit-llvm -o - %s | FileCheck --check-prefix=BRANCH1 %s
RUN: m2lang -DBRANCH=b2 -filetype=asm -emit-llvm -o - %s | FileCheck --check-prefix=BRANCH2 %s
RUN: m2lang -DBRANCH=b3 -filetype=asm -emit-llvm -o - %s | FileCheck --check-prefix=BRANCH3 %s
RUN: m2lang -DBRANCH=b2 -DNESTED=Y1 -filetype=asm -emit-llvm -o - %s | FileCheck --check-prefix=NESTED %s
*)
MODULE Nested;

<* ENVIRON(BRANCH, "b4") *>
<* ENVIRON(NESTED, FALSE) *>
<* IF BRANCH="b1" THEN*>
VAR X : INTEGER;
<* ELSIF BRANCH="b2" THEN*>
  VAR
  <* IF NESTED="Y1" THEN*>
    Y1
  <*ELSE*>
    Y2
  <*END*>
  : INTEGER;
<*ELSE*>
VAR Z : INTEGER;
<*END*>

(*
BRANCH1: @_m6Nested1X
BRANCH2: @_m6Nested2Y2
BRANCH3: @_m6Nested1Z
NESTED: @_m6Nested2Y1
*)
END Nested.
