var x: integer;
begin
  x := 314159265;
  x := bitnot(x);
  print_num(x); newline()
end.

(*<<
-314159266
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   x := 314159265;
CONST 314159265
STGW _x
!   x := bitnot(x);
LDGW _x
BITNOT
STGW _x
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
