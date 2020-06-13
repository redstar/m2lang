MODULE Mandelbrot;
(*
 * Computes a ascii based picture of a specified part of the mandelbrot set.
 *
 * Change define values, then recompile:
 * MAX_ITERATION, ..., RESOLUTION
 *
 * Based on the sample program mandelbrot2.c in the AIX 5L Porting Guide
 * http://www.redbooks.ibm.com/abstracts/sg246034.html?Open
 *
 *)

IMPORT STextIO;

(* Values can be changed for different output. *)
CONST
  MAX_ITERATION = 262144;
  MAX_LENGTH = 100.0;
  X_MIN = -2.1;
  Y_MIN = -1.1;
  X_MAX = 0.7;
  Y_MAX = 1.1;
  RESOLUTION = 24; (* Vertical resolution, horizontal is then derived *)

(* Do not change the following values. *)
CONST
  COLORS = " -:=+oxOX@#";
  xres = TRUNC(LFLOAT(RESOLUTION)*3.2)-1;
  yres = RESOLUTION-1;
  xmin = X_MIN;
  ymin = Y_MIN;
  xstep = (X_MAX - X_MIN) / (LFLOAT(RESOLUTION)*3.2);
  ystep = (Y_MAX - Y_MIN) / LFLOAT(RESOLUTION);

VAR
  pixels: ARRAY [0..yres] OF ARRAY [0..xres] OF CARDINAL;

(*****************************************
** Compute row specified in y
******************************************)

PROCEDURE Row(y: CARDINAL);
VAR
  x: CARDINAL;
  iteration: CARDINAL;
  z1, z2, t1: LONGREAL;
  cindx, exp : CARDINAL;
BEGIN
  FOR x := 0 TO xres DO
    iteration := 0;
    z1 := 0.0;
    z2 := 0.0;
    REPEAT
      t1 := z1*z1 - z2*z2 + (xmin + LFLOAT(x)*xstep);
      z2 := 2.0 * z1*z2 + (ymin + LFLOAT(y)*ystep);
      z1 := t1;
      INC(iteration);
    UNTIL (iteration >= MAX_ITERATION) OR (z1*z1 + z2*z2 >= MAX_LENGTH);
    cindx := 0;
    exp := 1;
    REPEAT
      INC(cindx);
      exp := 2*exp;
    UNTIL exp >= iteration;
    IF iteration >= MAX_ITERATION THEN
      pixels[y][x] := 0;
    ELSE
      pixels[y][x] := cindx MOD LENGTH(COLORS);
    END
  END
END Row;

VAR
  x, y: CARDINAL;
BEGIN
  (* Compute a row at the time *)
  FOR y := 0 TO yres DO
    Row(y);
  END;

  (* Print out fractal pixels *)
  FOR y := 0 TO yres DO
    FOR x := 0 TO xres DO
      STextIO.WriteChar(COLORS[pixels[y][x]])
    END;
    STextIO.WriteLn
  END
END Mandelbrot.
