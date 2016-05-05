var x, y, z: integer;

begin
  x := 3;
  y := 5;
  z := x * y;
  print_num(z);
  newline();
end.

(*<<
15
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   x := 3;
CONST 3
STGW _x
!   y := 5;
CONST 5
STGW _y
!   z := x * y;
LDGW _x
LDGW _y
TIMES
STGW _z
!   print_num(z);
LDGW _z
CONST 0
GLOBAL _print_num
PCALL 1
!   newline();
CONST 0
GLOBAL _newline
PCALL 0
! end.
RETURN
END

GLOVAR _x 4
GLOVAR _y 4
GLOVAR _z 4
! End
]]*)
