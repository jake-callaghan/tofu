(* fib.p *)

(* fib -- fibonacci numbers *)
proc fib(n: integer): integer;
begin
  if n <= 1 then 
    return 1 
  else 
    return fib(n-1) + fib(n-2)
  end
end;

begin
  print_num(fib(6)); newline()
end.

(*<<
13
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _fib 0 0 0
!   if n <= 1 then 
LDLW 16
CONST 1
JGT L2
!     return 1 
CONST 1
RETURNW
LABEL L2
!     return fib(n-1) + fib(n-2)
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _fib
PCALLW 1
LDLW 16
CONST 2
MINUS
CONST 0
GLOBAL _fib
PCALLW 1
PLUS
RETURNW
END

PROC MAIN 0 0 0
!   print_num(fib(6)); newline()
CONST 6
CONST 0
GLOBAL _fib
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
