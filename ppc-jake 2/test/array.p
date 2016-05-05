(* array.p *)

var i: integer;

var a: array 10 of integer;

proc foo();
  var j: integer;
  var b : array 10 of integer;
begin
  print_string("foo"); newline();
  j := 2; b[0] := 1; b[1] := 1;
  while 10 > j do
    b[j] := b[j-2] + b[j-1];
    print_char(' '); print_num(b[j]);
    j := 1+j
  end;
  newline();
end;

begin
  print_string("baz"); newline();
  i := 2; a[0] := 1; a[1] := 1;
  while i < 10 do
    a[i] := a[i-2] + a[i-1];
    print_char(' '); print_num(a[i]);
    i := i+1
  end;
  newline();
  foo()
end.

(*<<
baz
 2 3 5 8 13 21 34 55
foo
 2 3 5 8 13 21 34 55
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _foo 44 0 0
!   print_string("foo"); newline();
CONST 3
GLOBAL g1
CONST 0
GLOBAL _print_string
PCALL 2
CONST 0
GLOBAL _newline
PCALL 0
!   j := 2; b[0] := 1; b[1] := 1;
CONST 2
STLW -4
CONST 1
STLW -44
CONST 1
STLW -40
!   while 10 > j do
JUMP L4
LABEL L3
!     b[j] := b[j-2] + b[j-1];
LOCAL -44
LDLW -4
CONST 2
MINUS
LDIW
LOCAL -44
LDLW -4
CONST 1
MINUS
LDIW
PLUS
LOCAL -44
LDLW -4
STIW
!     print_char(' '); print_num(b[j]);
CONST 32
CONST 0
GLOBAL _print_char
PCALL 1
LOCAL -44
LDLW -4
LDIW
CONST 0
GLOBAL _print_num
PCALL 1
!     j := 1+j
CONST 1
LDLW -4
PLUS
STLW -4
LABEL L4
CONST 10
LDLW -4
JGT L3
!   newline();
CONST 0
GLOBAL _newline
PCALL 0
! end;
RETURN
END

PROC MAIN 0 0 0
!   print_string("baz"); newline();
CONST 3
GLOBAL g2
CONST 0
GLOBAL _print_string
PCALL 2
CONST 0
GLOBAL _newline
PCALL 0
!   i := 2; a[0] := 1; a[1] := 1;
CONST 2
STGW _i
CONST 1
STGW _a
CONST 1
GLOBAL _a
STNW 4
!   while i < 10 do
JUMP L7
LABEL L6
!     a[i] := a[i-2] + a[i-1];
GLOBAL _a
LDGW _i
CONST 2
MINUS
LDIW
GLOBAL _a
LDGW _i
CONST 1
MINUS
LDIW
PLUS
GLOBAL _a
LDGW _i
STIW
!     print_char(' '); print_num(a[i]);
CONST 32
CONST 0
GLOBAL _print_char
PCALL 1
GLOBAL _a
LDGW _i
LDIW
CONST 0
GLOBAL _print_num
PCALL 1
!     i := i+1
LDGW _i
CONST 1
PLUS
STGW _i
LABEL L7
LDGW _i
CONST 10
JLT L6
!   newline();
CONST 0
GLOBAL _newline
PCALL 0
!   foo()
CONST 0
GLOBAL _foo
PCALL 0
RETURN
END

GLOVAR _i 4
GLOVAR _a 40
! String "foo"
DEFINE g1
STRING 666F6F00

! String "baz"
DEFINE g2
STRING 62617A00

! End
]]*)
