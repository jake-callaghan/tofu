(* Pascal's triangle *)

const n = 10;

proc pascal2();
  var i, j: integer;
  var a: array n of array n+1 of integer;
begin
  i := 0;
  while i < n do
    a[i][0] := 1; j := 1;
    print_num(a[i][0]);
    while j <= i do
      a[i][j] := a[i-1][j-1] + a[i-1][j];
      print_char(' '); print_num(a[i][j]);
      j := j+1
    end;
    a[i][i+1] := 0;
    newline();
    i := i+1
  end
end;

begin
  pascal2()
end.

(*<<
1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1
1 7 21 35 35 21 7 1
1 8 28 56 70 56 28 8 1
1 9 36 84 126 126 84 36 9 1
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _pascal2 448 0 0
!   i := 0;
CONST 0
STLW -4
!   while i < n do
JUMP L2
LABEL L1
!     a[i][0] := 1; j := 1;
CONST 1
LOCAL -448
LDLW -4
CONST 44
TIMES
PLUSA
STOREW
CONST 1
STLW -8
!     print_num(a[i][0]);
LOCAL -448
LDLW -4
CONST 44
TIMES
PLUSA
LOADW
CONST 0
GLOBAL _print_num
PCALL 1
!     while j <= i do
JUMP L5
LABEL L4
!       a[i][j] := a[i-1][j-1] + a[i-1][j];
LOCAL -448
LDLW -4
CONST 1
MINUS
CONST 44
TIMES
PLUSA
LDLW -8
CONST 1
MINUS
LDIW
LOCAL -448
LDLW -4
CONST 1
MINUS
CONST 44
TIMES
PLUSA
LDLW -8
LDIW
PLUS
LOCAL -448
LDLW -4
CONST 44
TIMES
PLUSA
LDLW -8
STIW
!       print_char(' '); print_num(a[i][j]);
CONST 32
CONST 0
GLOBAL _print_char
PCALL 1
LOCAL -448
LDLW -4
CONST 44
TIMES
PLUSA
LDLW -8
LDIW
CONST 0
GLOBAL _print_num
PCALL 1
!       j := j+1
LDLW -8
CONST 1
PLUS
STLW -8
LABEL L5
LDLW -8
LDLW -4
JLEQ L4
!     a[i][i+1] := 0;
CONST 0
LOCAL -448
LDLW -4
CONST 44
TIMES
PLUSA
LDLW -4
CONST 1
PLUS
STIW
!     newline();
CONST 0
GLOBAL _newline
PCALL 0
!     i := i+1
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

PROC MAIN 0 0 0
!   pascal2()
CONST 0
GLOBAL _pascal2
PCALL 0
RETURN
END

! End
]]*)
