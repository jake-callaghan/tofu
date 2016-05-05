type r = record x: integer; q: record y, z: integer; end; end;

var m: r;

begin
  m.x := 2; m.q.y := 3; m.q.z := 4;
  print_num(m.x+m.q.y+m.q.z); newline()
end.

(*<<
9
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   m.x := 2; m.q.y := 3; m.q.z := 4;
CONST 2
STGW _m
CONST 3
GLOBAL _m
STNW 4
CONST 4
GLOBAL _m
STNW 8
!   print_num(m.x+m.q.y+m.q.z); newline()
LDGW _m
GLOBAL _m
LDNW 4
PLUS
GLOBAL _m
LDNW 8
PLUS
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _m 12
! End
]]*)
