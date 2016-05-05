var x, y: integer;

proc f(n: integer): integer;
begin x := x + n; return x end;

begin
  x := 2;
  y := f(3) + 1;
  y := f(3) + 1;
  print_num(x); newline()
end.

(*<<
8
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _f 0 0 0
! begin x := x + n; return x end;
LDGW _x
LDLW 16
PLUS
STGW _x
LDGW _x
RETURNW
END

PROC MAIN 0 0 0
!   x := 2;
CONST 2
STGW _x
!   y := f(3) + 1;
CONST 3
CONST 0
GLOBAL _f
PCALLW 1
CONST 1
PLUS
STGW _y
!   y := f(3) + 1;
CONST 3
CONST 0
GLOBAL _f
PCALLW 1
CONST 1
PLUS
STGW _y
!   print_num(x); newline()
LDGW _x
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _x 4
GLOVAR _y 4
! End
]]*)
