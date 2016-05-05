proc f(): integer;
  var x, y: integer;
begin
  x := 3;
  y := x + 1;
  g();
  return x + 1
end;

proc g(); begin end;

begin
  print_num(f()); newline()
end.

(*<<
4
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _f 8 0 0
!   x := 3;
CONST 3
STLW -4
!   y := x + 1;
LDLW -4
CONST 1
PLUS
STLW -8
!   g();
CONST 0
GLOBAL _g
PCALL 0
!   return x + 1
LDLW -4
CONST 1
PLUS
RETURNW
END

PROC _g 0 0 0
! proc g(); begin end;
RETURN
END

PROC MAIN 0 0 0
!   print_num(f()); newline()
CONST 0
GLOBAL _f
PCALLW 0
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! End
]]*)
