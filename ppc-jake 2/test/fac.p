(* fac.p *)

proc fac(n: integer): integer;
begin
  if n = 0 then
    return 1
  else
    return n * fac(n-1)
  end
end;

var f: integer;

begin
  f := fac(10);
  print_num(f);
  newline()
end.
        
(*<<
3628800
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _fac 0 0 0
!   if n = 0 then
LDLW 16
JEQZ L1
JUMP L2
LABEL L1
!     return 1
CONST 1
RETURNW
LABEL L2
!     return n * fac(n-1)
LDLW 16
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _fac
PCALLW 1
TIMES
RETURNW
END

PROC MAIN 0 0 0
!   f := fac(10);
CONST 10
CONST 0
GLOBAL _fac
PCALLW 1
STGW _f
!   print_num(f);
LDGW _f
CONST 0
GLOBAL _print_num
PCALL 1
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _f 4
! End
]]*)
