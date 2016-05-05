(* strcopy.p *)

const in = "Hello, world!*";

var out: array 128 of char; i: integer;

begin
  i := 0;
  while in[i] <> '*' do
    out[i] := in[i];
    i := i + 1
  end;
  out[i] := chr(0);
  print_string(out); newline()
end.

(*<<
Hello, world!
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC MAIN 0 0 0
!   i := 0;
CONST 0
STGW _i
!   while in[i] <> '*' do
JUMP L3
LABEL L2
!     out[i] := in[i];
GLOBAL g1
LDGW _i
LDIC
GLOBAL _out
LDGW _i
STIC
!     i := i + 1
LDGW _i
CONST 1
PLUS
STGW _i
LABEL L3
GLOBAL g1
LDGW _i
LDIC
CONST 42
JNEQ L2
!   out[i] := chr(0);
CONST 0
GLOBAL _out
LDGW _i
STIC
!   print_string(out); newline()
CONST 128
GLOBAL _out
CONST 0
GLOBAL _print_string
PCALL 2
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _out 128
GLOVAR _i 4
! String "Hello, world!*"
DEFINE g1
STRING 48656C6C6F2C20776F726C64212A00

! End
]]*)
