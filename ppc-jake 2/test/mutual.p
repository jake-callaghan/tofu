(* mutual.p *)

proc flip(i: integer): integer;
  var r: integer;
begin
  if i = 0 then 
    r := 1
  else 
    r := 2 * flop(i-1)
  end;
  print_string("flip("); print_num(i); 
  print_string(") = "); print_num(r);
  newline();
  return r
end;

proc flop(i: integer): integer;
  var r: integer;
begin
  if i = 0 then 
    r := 1
  else 
    r := flip(i-1) + k
  end;
  print_string("flop("); print_num(i); 
  print_string(") = "); print_num(r);
  newline();
  return r
end;

const k = 5;

begin
  print_num(flip(5));
  newline()
end.

(*<<
flop(0) = 1
flip(1) = 2
flop(2) = 7
flip(3) = 14
flop(4) = 19
flip(5) = 38
38
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _flip 4 0 0
!   if i = 0 then 
LDLW 16
JEQZ L5
JUMP L6
LABEL L5
!     r := 1
CONST 1
STLW -4
JUMP L7
LABEL L6
!     r := 2 * flop(i-1)
CONST 2
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _flop
PCALLW 1
TIMES
STLW -4
LABEL L7
!   print_string("flip("); print_num(i); 
CONST 5
GLOBAL g1
CONST 0
GLOBAL _print_string
PCALL 2
LDLW 16
CONST 0
GLOBAL _print_num
PCALL 1
!   print_string(") = "); print_num(r);
CONST 4
GLOBAL g2
CONST 0
GLOBAL _print_string
PCALL 2
LDLW -4
CONST 0
GLOBAL _print_num
PCALL 1
!   newline();
CONST 0
GLOBAL _newline
PCALL 0
!   return r
LDLW -4
RETURNW
END

PROC _flop 4 0 0
!   if i = 0 then 
LDLW 16
JEQZ L8
JUMP L9
LABEL L8
!     r := 1
CONST 1
STLW -4
JUMP L10
LABEL L9
!     r := flip(i-1) + k
LDLW 16
CONST 1
MINUS
CONST 0
GLOBAL _flip
PCALLW 1
CONST 5
PLUS
STLW -4
LABEL L10
!   print_string("flop("); print_num(i); 
CONST 5
GLOBAL g3
CONST 0
GLOBAL _print_string
PCALL 2
LDLW 16
CONST 0
GLOBAL _print_num
PCALL 1
!   print_string(") = "); print_num(r);
CONST 4
GLOBAL g4
CONST 0
GLOBAL _print_string
PCALL 2
LDLW -4
CONST 0
GLOBAL _print_num
PCALL 1
!   newline();
CONST 0
GLOBAL _newline
PCALL 0
!   return r
LDLW -4
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

! String "flip("
DEFINE g1
STRING 666C69702800

! String ") = "
DEFINE g2
STRING 29203D2000

! String "flop("
DEFINE g3
STRING 666C6F702800

! String ") = "
DEFINE g4
STRING 29203D2000

! End
]]*)
