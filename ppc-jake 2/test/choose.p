proc choose(n, k: integer): integer;
begin
  if n = 0 then
    if k = 0 then
      return 1
    else
      return 0
    end
  else
    return choose(n-1, k-1) + choose(n-1, k)
  end
end;

begin
  print_num(choose(6,4));
  newline()
end.

(*<<
15
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _choose 0 0 0
!   if n = 0 then
LDLW 16
JEQZ L1
JUMP L2
LABEL L1
!     if k = 0 then
LDLW 20
JEQZ L4
JUMP L5
LABEL L4
!       return 1
CONST 1
RETURNW
LABEL L5
!       return 0
CONST 0
RETURNW
LABEL L2
!     return choose(n-1, k-1) + choose(n-1, k)
LDLW 20
CONST 1
MINUS
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _choose
PCALLW 2
LDLW 20
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _choose
PCALLW 2
PLUS
RETURNW
END

PROC MAIN 0 0 0
!   print_num(choose(6,4));
CONST 4
CONST 6
CONST 0
GLOBAL _choose
PCALLW 2
CONST 0
GLOBAL _print_num
PCALL 1
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! End
]]*)
