(* sumpower.p *)

proc sum(a, b: integer; proc f(x: integer): integer): integer;
  var i, s: integer;
begin
  i := a; s := 0;
  while i <= b do
    s := s + f(i);
    i := i + 1
  end;
  return s
end;

proc sum_powers(a, b, n: integer): integer;
  proc pow(x: integer): integer;
    var j, p: integer;
  begin
    j := 0; p := 1;
    while j < n do
      p := p * x;
      j := j + 1
    end;
    return p
  end;
begin
  return sum(a, b, pow)
end;

begin
  print_num(sum_powers(1, 10, 3));
  newline()
end.

(*<<
3025
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _sum 8 0 0
!   i := a; s := 0;
LDLW 16
STLW -4
CONST 0
STLW -8
!   while i <= b do
JUMP L2
LABEL L1
!     s := s + f(i);
LDLW -8
LDLW -4
LDLW 28
LDLW 24
PCALLW 1
PLUS
STLW -8
!     i := i + 1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L2
LDLW -4
LDLW 20
JLEQ L1
!   return s
LDLW -8
RETURNW
END

PROC _sum_powers 0 0 0
!   return sum(a, b, pow)
LOCAL 0
GLOBAL _pow
LDLW 20
LDLW 16
CONST 0
GLOBAL _sum
PCALLW 4
RETURNW
END

PROC _pow 8 0 0
!     j := 0; p := 1;
CONST 0
STLW -4
CONST 1
STLW -8
!     while j < n do
JUMP L5
LABEL L4
!       p := p * x;
LDLW -8
LDLW 16
TIMES
STLW -8
!       j := j + 1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L5
LDLW -4
LDLW 12
LDNW 24
JLT L4
!     return p
LDLW -8
RETURNW
END

PROC MAIN 0 0 0
!   print_num(sum_powers(1, 10, 3));
CONST 3
CONST 10
CONST 1
CONST 0
GLOBAL _sum_powers
PCALLW 3
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
