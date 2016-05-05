(* twice.p *)

type int = integer;

proc square(x: int): int; begin return x * x end;

proc twice(proc f(y: int): int; x: int): int;
begin return f(f(x)) end;

proc ap_to_sq(proc ff(proc f(x: int): int; x: int): int; x: int): int;
begin return ff(square, x) end;

begin
  print_num(ap_to_sq(twice, 3));
  newline()
end.

(*<<
81
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _square 0 0 0
! proc square(x: int): int; begin return x * x end;
LDLW 16
LDLW 16
TIMES
RETURNW
END

PROC _twice 0 0 0
! begin return f(f(x)) end;
LDLW 24
LDLW 20
LDLW 16
PCALLW 1
LDLW 20
LDLW 16
PCALLW 1
RETURNW
END

PROC _ap_to_sq 0 0 0
! begin return ff(square, x) end;
LDLW 24
CONST 0
GLOBAL _square
LDLW 20
LDLW 16
PCALLW 3
RETURNW
END

PROC MAIN 0 0 0
!   print_num(ap_to_sq(twice, 3));
CONST 3
CONST 0
GLOBAL _twice
CONST 0
GLOBAL _ap_to_sq
PCALLW 3
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
