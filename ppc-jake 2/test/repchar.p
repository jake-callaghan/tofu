proc repchar(c: char; n: integer);
  var i: integer;
begin
  if n > 0 then
    print_char(c);
    repchar(c, n-1)
  end
end;

begin
  repchar('A', 3);
  repchar('B', 5);
  newline()
end.

(*<<
AAABBBBB
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _repchar 4 0 0
!   if n > 0 then
LDLW 20
JGTZ L1
JUMP L3
LABEL L1
!     print_char(c);
LDLC 16
CONST 0
GLOBAL _print_char
PCALL 1
!     repchar(c, n-1)
LDLW 20
CONST 1
MINUS
LDLC 16
CONST 0
GLOBAL _repchar
PCALL 2
LABEL L3
RETURN
END

PROC MAIN 0 0 0
!   repchar('A', 3);
CONST 3
CONST 65
CONST 0
GLOBAL _repchar
PCALL 2
!   repchar('B', 5);
CONST 5
CONST 66
CONST 0
GLOBAL _repchar
PCALL 2
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! End
]]*)
