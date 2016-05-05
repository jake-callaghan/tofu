type vector = array 10 of integer;

proc dovec(proc f(x: integer); var v: vector);
  var i: integer;
begin
  i := 0;
  while i < 10 do 
    f(v[i]); i := i+1 
  end
end;

proc sum(var v: vector): integer;
  var s: integer;

  proc add(x: integer);
  begin
    s := s + x
  end;

begin
  s := 0;
  dovec(add, v);
  return s
end;

var a: vector; i: integer;

begin
  i := 0;
  while i < 10 do
    a[i] := (i+1)*(i+1);
    i := i+1
  end;
  print_num(sum(a));
  newline()
end.

(*<<
385
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _dovec 4 0 0
!   i := 0;
CONST 0
STLW -4
!   while i < 10 do 
JUMP L2
LABEL L1
!     f(v[i]); i := i+1 
LDLW 24
LDLW -4
LDIW
LDLW 20
LDLW 16
PCALL 1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L2
LDLW -4
CONST 10
JLT L1
RETURN
END

PROC _sum 4 0 0
!   s := 0;
CONST 0
STLW -4
!   dovec(add, v);
LDLW 16
LOCAL 0
GLOBAL _add
CONST 0
GLOBAL _dovec
PCALL 3
!   return s
LDLW -4
RETURNW
END

PROC _add 0 0 0
!     s := s + x
LDLW 12
LDNW -4
LDLW 16
PLUS
LDLW 12
STNW -4
RETURN
END

PROC MAIN 0 0 0
!   i := 0;
CONST 0
STGW _i
!   while i < 10 do
JUMP L5
LABEL L4
!     a[i] := (i+1)*(i+1);
LDGW _i
CONST 1
PLUS
LDGW _i
CONST 1
PLUS
TIMES
GLOBAL _a
LDGW _i
STIW
!     i := i+1
LDGW _i
CONST 1
PLUS
STGW _i
LABEL L5
LDGW _i
CONST 10
JLT L4
!   print_num(sum(a));
GLOBAL _a
CONST 0
GLOBAL _sum
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

GLOVAR _a 40
GLOVAR _i 4
! End
]]*)
