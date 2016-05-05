proc flip(x: integer): integer;
  proc flop(y: integer): integer;
  begin
    if y = 0 then return 1 else return flip(y-1) + x end
  end;
begin
  if x = 0 then return 1 else return 2 * flop(x-1) end
end;

begin
  print_num(flip(5));
  newline()
end.

(* flip(5) = 2 * flop(4) = 2 * (flip(3) + 5)
    = 4 * flop(2) + 10 = 4 * (flip(1) + 3) + 10
    = 8 * flop(0) + 22 = 30 *)

(*<<
30
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _flip 0 0 0
!   if x = 0 then return 1 else return 2 * flop(x-1) end
LDLW 16
JEQZ L1
JUMP L2
LABEL L1
CONST 1
RETURNW
LABEL L2
CONST 2
LDLW 16
CONST 1
MINUS
LOCAL 0
GLOBAL _flop
PCALLW 1
TIMES
RETURNW
END

PROC _flop 0 0 0
!     if y = 0 then return 1 else return flip(y-1) + x end
LDLW 16
JEQZ L4
JUMP L5
LABEL L4
CONST 1
RETURNW
LABEL L5
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _flip
PCALLW 1
LDLW 12
LDNW 16
PLUS
RETURNW
END

PROC MAIN 0 0 0
!   print_num(flip(5));
CONST 5
CONST 0
GLOBAL _flip
PCALLW 1
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
