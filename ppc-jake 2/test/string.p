(* hello.p *)
begin
  print_string("Hello world!");
  newline()
end.

(*<<
Hello world!
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   print_string("Hello world!");
CONST 12
GLOBAL g1
CONST 0
GLOBAL _print_string
PCALL 2
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! String "Hello world!"
DEFINE g1
STRING 48656C6C6F20776F726C642100

! End
]]*)
