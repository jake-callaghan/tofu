proc foo();
begin
  while true do newline() end
end;

begin end.

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _foo 0 0 0
LABEL L1
!   while true do newline() end
CONST 0
GLOBAL _newline
PCALL 0
JUMP L1
END

PROC MAIN 0 0 0
! begin end.
RETURN
END

! End
]]*)
