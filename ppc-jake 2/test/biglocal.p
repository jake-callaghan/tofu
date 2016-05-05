proc foo(var a: array 10000 of integer);
begin a[5000] := 3 end;

var b: array  10000 of integer;
begin foo(b) end.

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _foo 0 0 0
! begin a[5000] := 3 end;
CONST 3
LDLW 16
STNW 20000
RETURN
END

PROC MAIN 0 0 0
! begin foo(b) end.
GLOBAL _b
CONST 0
GLOBAL _foo
PCALL 1
RETURN
END

GLOVAR _b 40000
! End
]]*)
