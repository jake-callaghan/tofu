type ptr = pointer to rec;
  rec = record 
      data: integer; 
      next: ptr; 
    end;

proc sum(p: ptr): integer;
  var q: ptr; s: integer;
begin
  q := p; s := 0;
  while q <> nil do
    s := s + q^.data;
    q := q^.next
  end;
  return s
end;

proc main();
  const input = "3141592650";
  var i: integer; p, q: ptr;
begin
  i := 0;
  while input[i] <> '0' do i := i+1 end;

  p := nil;
  while i > 0 do
    i := i-1;
    q := p;
    new(p);
    p^.data := ord(input[i]) - ord('0');
    p^.next := q
  end;

  print_num(sum(p)); newline()
end;

begin
  main()
end.

(*<<
36
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _sum 8 0 0
!   q := p; s := 0;
LDLW 16
STLW -4
CONST 0
STLW -8
!   while q <> nil do
JUMP L3
LABEL L2
!     s := s + q^.data;
LDLW -8
LDLW -4
LOADW
PLUS
STLW -8
!     q := q^.next
LDLW -4
LDNW 4
STLW -4
LABEL L3
LDLW -4
JNEQZ L2
!   return s
LDLW -8
RETURNW
END

PROC _main 12 0 0
!   i := 0;
CONST 0
STLW -4
!   while input[i] <> '0' do i := i+1 end;
JUMP L6
LABEL L5
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L6
GLOBAL g1
LDLW -4
LDIC
CONST 48
JNEQ L5
!   p := nil;
CONST 0
STLW -8
!   while i > 0 do
JUMP L9
LABEL L8
!     i := i-1;
LDLW -4
CONST 1
MINUS
STLW -4
!     q := p;
LDLW -8
STLW -12
!     new(p);
CONST 8
LOCAL -8
CONST 0
GLOBAL _new
PCALL 2
!     p^.data := ord(input[i]) - ord('0');
GLOBAL g1
LDLW -4
LDIC
CONST 48
MINUS
LDLW -8
STOREW
!     p^.next := q
LDLW -12
LDLW -8
STNW 4
LABEL L9
LDLW -4
JGTZ L8
!   print_num(sum(p)); newline()
LDLW -8
CONST 0
GLOBAL _sum
PCALLW 1
CONST 0
GLOBAL _print_num
PCALL 1
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

PROC MAIN 0 0 0
!   main()
CONST 0
GLOBAL _main
PCALL 0
RETURN
END

! String "3141592650"
DEFINE g1
STRING 3331343135393236353000

! End
]]*)
