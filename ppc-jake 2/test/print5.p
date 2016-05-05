begin
  print_string("five"); newline()
end.

(*<<
five
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   print_string("five"); newline()
CONST 4
GLOBAL g1
CONST 0
GLOBAL _print_string
PCALL 2
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! String "five"
DEFINE g1
STRING 6669766500

! End
]]*)
