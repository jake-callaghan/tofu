proc search(k, n: integer; proc avail(x: integer): boolean);
  var d, n1: integer;
  proc avail1(x: integer): boolean;
  begin
    return (x <> d) and avail(x)
  end;
begin
  if k = 9 then
    print_num(n); newline()
  else
    d := 1;
    while d < 10 do
      n1 := 10 * n + d;
      if (n1 mod (k+1) = 0) and avail(d) then
        search(k+1, n1, avail1)
      end;
      d := d+1
    end
  end
end;

proc avail0(x: integer): boolean;
begin
  return true
end;

begin
  search(0, 0, avail0)
end.

(*<<
381654729
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _search 8 0 0
!   if k = 9 then
LDLW 16
CONST 9
JNEQ L2
!     print_num(n); newline()
LDLW 20
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
JUMP L3
LABEL L2
!     d := 1;
CONST 1
STLW -4
!     while d < 10 do
JUMP L5
LABEL L4
!       n1 := 10 * n + d;
CONST 10
LDLW 20
TIMES
LDLW -4
PLUS
STLW -8
!       if (n1 mod (k+1) = 0) and avail(d) then
LDLW -8
LDLW 16
CONST 1
PLUS
MOD
JEQZ L10
JUMP L9
LABEL L10
LDLW -4
LDLW 28
LDLW 24
PCALLW 1
JNEQZ L7
JUMP L9
LABEL L7
!         search(k+1, n1, avail1)
LOCAL 0
GLOBAL _avail1
LDLW -8
LDLW 16
CONST 1
PLUS
CONST 0
GLOBAL _search
PCALL 4
LABEL L9
!       d := d+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L5
LDLW -4
CONST 10
JLT L4
LABEL L3
RETURN
END

PROC _avail1 0 0 0
!     return (x <> d) and avail(x)
LDLW 16
LDLW 12
LDNW -4
JEQ L12
LDLW 16
LDLW 12
LDNW 28
LDLW 12
LDNW 24
PCALLW 1
JNEQZ L11
JUMP L12
LABEL L11
CONST 1
JUMP L13
LABEL L12
CONST 0
LABEL L13
RETURNW
END

PROC _avail0 0 0 0
!   return true
CONST 1
RETURNW
END

PROC MAIN 0 0 0
!   search(0, 0, avail0)
CONST 0
GLOBAL _avail0
CONST 0
CONST 0
CONST 0
GLOBAL _search
PCALL 4
RETURN
END

! End
]]*)
