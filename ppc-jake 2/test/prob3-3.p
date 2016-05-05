proc double(x: integer): integer;
begin
  return x + x
end;

proc apply3(proc f(x:integer): integer): integer;
begin
  return f(3)
end;

begin
  print_num(apply3(double));
  newline()
end.

(*<<
6
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _double 0 0 0
!   return x + x
LDLW 16
LDLW 16
PLUS
RETURNW
END

PROC _apply3 0 0 0
!   return f(3)
CONST 3
LDLW 20
LDLW 16
PCALLW 1
RETURNW
END

PROC MAIN 0 0 0
!   print_num(apply3(double));
CONST 0
GLOBAL _double
CONST 0
GLOBAL _apply3
PCALLW 2
CONST 0
GLOBAL _print_num
PCALL 1
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! End
]]*)
