const wordlen = 10;

type string = array wordlen of char;

type rec = record name: string; age: integer end;

var 
  db: array 20 of rec;
  N: integer;

proc equal(x, y: string): boolean;
  var i: integer;
begin
  i := 0;
  while i < wordlen do
    if x[i] <> y[i] then
      return false
    end;
    i := i+1
  end;
  return true
end;

proc copy(var dst: string; src: string);
  var i: integer;
begin
  i := 0;
  while i < wordlen do
    dst[i] := src[i]; i := i+1
  end
end;

proc store(n: string; a: integer);
begin
  copy(db[N].name, n);
  db[N].age := a;
  N := N+1
end;

proc recall(n: string): integer;
  var i: integer;
begin
  i := 0;
  while i < N do
    if equal(db[i].name, n) then
      return db[i].age
    end;
    i := i+1
  end;
  return 999
end;

begin
  N := 0;

  store("bill      ", 23);
  store("george    ", 34);

  print_num(recall("george    ")); newline();
  print_num(recall("fred      ")); newline()
end.

(*<<
34
999
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _equal 4 0 0
!   i := 0;
CONST 0
STLW -4
!   while i < wordlen do
JUMP L6
LABEL L5
!     if x[i] <> y[i] then
LDLW 16
LDLW -4
LDIC
LDLW 20
LDLW -4
LDIC
JEQ L10
!       return false
CONST 0
RETURNW
LABEL L10
!     i := i+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L6
LDLW -4
CONST 10
JLT L5
!   return true
CONST 1
RETURNW
END

PROC _copy 4 0 0
!   i := 0;
CONST 0
STLW -4
!   while i < wordlen do
JUMP L12
LABEL L11
!     dst[i] := src[i]; i := i+1
LDLW 20
LDLW -4
LDIC
LDLW 16
LDLW -4
STIC
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L12
LDLW -4
CONST 10
JLT L11
RETURN
END

PROC _store 0 0 0
!   copy(db[N].name, n);
LDLW 16
GLOBAL _db
LDGW _N
CONST 16
TIMES
PLUSA
CONST 0
GLOBAL _copy
PCALL 2
!   db[N].age := a;
LDLW 20
GLOBAL _db
LDGW _N
CONST 16
TIMES
PLUSA
STNW 12
!   N := N+1
LDGW _N
CONST 1
PLUS
STGW _N
RETURN
END

PROC _recall 4 0 0
!   i := 0;
CONST 0
STLW -4
!   while i < N do
JUMP L15
LABEL L14
!     if equal(db[i].name, n) then
LDLW 16
GLOBAL _db
LDLW -4
CONST 16
TIMES
PLUSA
CONST 0
GLOBAL _equal
PCALLW 2
JNEQZ L17
JUMP L19
LABEL L17
!       return db[i].age
GLOBAL _db
LDLW -4
CONST 16
TIMES
PLUSA
LDNW 12
RETURNW
LABEL L19
!     i := i+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L15
LDLW -4
LDGW _N
JLT L14
!   return 999
CONST 999
RETURNW
END

PROC MAIN 0 0 0
!   N := 0;
CONST 0
STGW _N
!   store("bill      ", 23);
CONST 23
GLOBAL g1
CONST 0
GLOBAL _store
PCALL 2
!   store("george    ", 34);
CONST 34
GLOBAL g2
CONST 0
GLOBAL _store
PCALL 2
!   print_num(recall("george    ")); newline();
GLOBAL g3
CONST 0
GLOBAL _recall
PCALLW 1
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
!   print_num(recall("fred      ")); newline()
GLOBAL g4
CONST 0
GLOBAL _recall
PCALLW 1
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

GLOVAR _db 320
GLOVAR _N 4
! String "bill      "
DEFINE g1
STRING 62696C6C20202020202000

! String "george    "
DEFINE g2
STRING 67656F7267652020202000

! String "george    "
DEFINE g3
STRING 67656F7267652020202000

! String "fred      "
DEFINE g4
STRING 6672656420202020202000

! End
]]*)
