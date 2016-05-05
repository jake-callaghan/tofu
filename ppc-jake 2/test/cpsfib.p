(* cpsfib.p *)

(* This program computes fibonacci numbers using the usual doubly
   recursive algorithm.  However, the algorithm has been transformed
   into continuation passing style.  A good test for procedural
   parameters! *)

(* fib -- fibonacci numbers *)
proc fib(n: integer): integer;

  (* fib1 -- continuation transformer for fib *)
  proc fib1(n: integer; proc k(r: integer): integer) : integer;
    proc k1(r1: integer): integer;
      proc k2(r2: integer): integer; begin return k(r1 + r2) end;
    begin return fib1(n-2, k2) end;
  begin
    if n <= 1 then 
      return k(1) 
    else 
      return fib1(n-1, k1)
    end
  end;

  (* id -- identity continuation *)
  proc id(r: integer): integer; begin return r end;

begin
  return fib1(n, id)
end;

begin
  print_num(fib(6)); newline()
end.

(*<<
13
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _fib 0 0 0
!   return fib1(n, id)
LOCAL 0
GLOBAL _id
LDLW 16
LOCAL 0
GLOBAL _fib1
PCALLW 3
RETURNW
END

PROC _fib1 0 0 0
!     if n <= 1 then 
LDLW 16
CONST 1
JGT L2
!       return k(1) 
CONST 1
LDLW 24
LDLW 20
PCALLW 1
RETURNW
LABEL L2
!       return fib1(n-1, k1)
LOCAL 0
GLOBAL _k1
LDLW 16
CONST 1
MINUS
LDLW 12
GLOBAL _fib1
PCALLW 3
RETURNW
END

PROC _k1 0 0 0
!     begin return fib1(n-2, k2) end;
LOCAL 0
GLOBAL _k2
LDLW 12
LDNW 16
CONST 2
MINUS
LDLW 12
LDNW 12
GLOBAL _fib1
PCALLW 3
RETURNW
END

PROC _k2 0 0 0
!       proc k2(r2: integer): integer; begin return k(r1 + r2) end;
LDLW 12
LDNW 16
LDLW 16
PLUS
LDLW 12
LDNW 12
LDNW 24
LDLW 12
LDNW 12
LDNW 20
PCALLW 1
RETURNW
END

PROC _id 0 0 0
!   proc id(r: integer): integer; begin return r end;
LDLW 16
RETURNW
END

PROC MAIN 0 0 0
!   print_num(fib(6)); newline()
CONST 6
CONST 0
GLOBAL _fib
PCALLW 1
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

! End
]]*)
