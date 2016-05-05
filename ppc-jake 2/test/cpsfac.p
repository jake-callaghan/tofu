(* cpsfac.p *)

proc fac(n: integer; proc k(r: integer): integer): integer;
  proc k1(r: integer): integer;
    var r1: integer;
  begin
    r1 := n * r;
    print_num(n); print_string(" * "); print_num(r);
    print_string(" = "); print_num(r1); newline();
    return k(r1) 
  end;
begin 
  if n = 0 then return k(1) else return fac(n-1, k1) end
end;

proc id(r: integer): integer;
begin 
  return r 
end;

begin 
  print_num(fac(10, id));
  newline()
end.

(*<<
1 * 1 = 1
2 * 1 = 2
3 * 2 = 6
4 * 6 = 24
5 * 24 = 120
6 * 120 = 720
7 * 720 = 5040
8 * 5040 = 40320
9 * 40320 = 362880
10 * 362880 = 3628800
3628800
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _fac 0 0 0
!   if n = 0 then return k(1) else return fac(n-1, k1) end
LDLW 16
JEQZ L3
JUMP L4
LABEL L3
CONST 1
LDLW 24
LDLW 20
PCALLW 1
RETURNW
LABEL L4
LOCAL 0
GLOBAL _k1
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _fac
PCALLW 3
RETURNW
END

PROC _k1 4 0 0
!     r1 := n * r;
LDLW 12
LDNW 16
LDLW 16
TIMES
STLW -4
!     print_num(n); print_string(" * "); print_num(r);
LDLW 12
LDNW 16
CONST 0
GLOBAL _print_num
PCALL 1
CONST 3
GLOBAL g1
CONST 0
GLOBAL _print_string
PCALL 2
LDLW 16
CONST 0
GLOBAL _print_num
PCALL 1
!     print_string(" = "); print_num(r1); newline();
CONST 3
GLOBAL g2
CONST 0
GLOBAL _print_string
PCALL 2
LDLW -4
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
!     return k(r1) 
LDLW -4
LDLW 12
LDNW 24
LDLW 12
LDNW 20
PCALLW 1
RETURNW
END

PROC _id 0 0 0
!   return r 
LDLW 16
RETURNW
END

PROC MAIN 0 0 0
!   print_num(fac(10, id));
CONST 0
GLOBAL _id
CONST 10
CONST 0
GLOBAL _fac
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

! String " * "
DEFINE g1
STRING 202A2000

! String " = "
DEFINE g2
STRING 203D2000

! End
]]*)
