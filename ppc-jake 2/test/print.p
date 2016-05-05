(* print.p *)

begin
  print_num(2); newline()
end.

(*<<
2
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   print_num(2); newline()
CONST 2
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
