(* baz.p *)

var x: integer;

proc baz(u: integer): integer;
begin
  x := u;
  return x
end;
  
begin
  print_num(baz(37)); newline();
  print_num(x); newline()
end.

(*<<
37
37
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _baz 0 0 0
!   x := u;
LDLW 16
STGW _x
!   return x
LDGW _x
RETURNW
END

PROC MAIN 0 0 0
!   print_num(baz(37)); newline();
CONST 37
CONST 0
GLOBAL _baz
PCALLW 1
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
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
! End
]]*)
