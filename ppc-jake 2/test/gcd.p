(* gcd.p *)

var x, y: integer;

begin
  x := 3 * 37; y := 5 * 37;
  while x <> y do
    if x > y then
      x := x - y
    else
      y := y - x
    end
  end;
  print_num(x); newline();
end.

(*<<
37
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   x := 3 * 37; y := 5 * 37;
CONST 111
STGW _x
CONST 185
STGW _y
!   while x <> y do
JUMP L2
LABEL L1
!     if x > y then
LDGW _x
LDGW _y
JLEQ L5
!       x := x - y
LDGW _x
LDGW _y
MINUS
STGW _x
JUMP L2
LABEL L5
!       y := y - x
LDGW _y
LDGW _x
MINUS
STGW _y
LABEL L2
LDGW _x
LDGW _y
JNEQ L1
!   print_num(x); newline();
LDGW _x
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
! end.
RETURN
END

GLOVAR _x 4
GLOVAR _y 4
! End
]]*)
