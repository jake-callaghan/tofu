var x: integer;

proc p();
var y, z: integer;
begin
  y := 1;
  z := y + 1;
  z := 3;
  z := y + 1;
  x := z
end;

begin
  p();
  print_num(x);
  newline()
end.

(*<<
2
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _p 8 0 0
!   y := 1;
CONST 1
STLW -4
!   z := y + 1;
LDLW -4
CONST 1
PLUS
STLW -8
!   z := 3;
CONST 3
STLW -8
!   z := y + 1;
LDLW -4
CONST 1
PLUS
STLW -8
!   x := z
LDLW -8
STGW _x
RETURN
END

PROC MAIN 0 0 0
!   p();
CONST 0
GLOBAL _p
PCALL 0
!   print_num(x);
LDGW _x
CONST 0
GLOBAL _print_num
PCALL 1
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _x 4
! End
]]*)
