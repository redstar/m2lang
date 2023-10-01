(*
RUN: m2lang -filetype=asm -emit-llvm -o - %s | FileCheck %s
TODO The array dereferencing is still wrong.
*)
MODULE Enum;

TYPE
  WeekDay = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  Month = (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

  DayInMonth = ARRAY Month OF WeekDay;

VAR
  Special: DayInMonth;

PROCEDURE SetDay(VAR d: WeekDay);
BEGIN
  d := Sun
END SetDay;
(*
CHECK-LABEL: _m4Enum6SetDay
CHECK: store i64 6, ptr %d, align 8
*)

PROCEDURE SetMonth(VAR m: DayInMonth);
VAR
  i: Month;
  j: WeekDay;
BEGIN
  j := Mon;
  FOR i := Jan TO Dec DO
    m[i] := j;
  END;
END SetMonth;
(*
CHECK-LABEL: _m4Enum8SetMonth
TODO Array access still wrong!
*)

PROCEDURE SetSpecial;
VAR
  i: Month;
  j: WeekDay;
BEGIN
  j := Mon;
  FOR i := Jan TO Dec DO
    Special[i] := j;
  END;
END SetSpecial;
(*
CHECK-LABEL: _m4Enum10SetSpecial
TODO Array access still wrong!
*)

END Enum.
