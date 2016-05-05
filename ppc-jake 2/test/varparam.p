(* varparam.p *)

proc one(var x: integer);
  proc two(); begin x := 37 end;
begin
  two()
end;

var y: integer;

begin
  one(y);
  print_num(y);
  newline()
end.

(*<<
37
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _one 0 0 0
!   two()
LOCAL 0
GLOBAL _two
PCALL 0
RETURN
END

PROC _two 0 0 0
!   proc two(); begin x := 37 end;
CONST 37
LDLW 12
LDNW 16
STOREW
RETURN
END

PROC MAIN 0 0 0
!   one(y);
GLOBAL _y
CONST 0
GLOBAL _one
PCALL 1
!   print_num(y);
LDGW _y
CONST 0
GLOBAL _print_num
PCALL 1
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _y 4
! End
]]*)
