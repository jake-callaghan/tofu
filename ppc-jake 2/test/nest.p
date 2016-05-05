proc f(x: integer): integer; begin return 2*x end;
begin print_num(f(f(3))); newline() end.

(*<<
12
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _f 0 0 0
! proc f(x: integer): integer; begin return 2*x end;
CONST 2
LDLW 16
TIMES
RETURNW
END

PROC MAIN 0 0 0
! begin print_num(f(f(3))); newline() end.
CONST 3
CONST 0
GLOBAL _f
PCALLW 1
CONST 0
GLOBAL _f
PCALLW 1
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
