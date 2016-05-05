(* N queens with array of choices *)

const N = 8;

proc queens(k: integer; var choice: array N of integer);
  var y, j, q: integer; ok: boolean;
begin
  if k = N then
    print(choice)
  else
    y := 0;
    while y < N do
      j := 0; ok := true;
      while ok and (j < k) do
	q := choice[j];
	ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
        j := j+1
      end;
      if ok then 
	choice[k] := y;
	queens(k+1, choice)
      end;
      y := y+1
    end
  end
end;

proc print(var choice: array N of integer);
  var x: integer;
begin
  x := 0;
  while x < N do
    print_num(choice[x]+1);
    x := x+1
  end;
  newline()
end;

var choice: array N of integer;

begin
  queens(0, choice)
end.

(*<<
15863724
16837425
17468253
17582463
24683175
25713864
25741863
26174835
26831475
27368514
27581463
28613574
31758246
35281746
35286471
35714286
35841726
36258174
36271485
36275184
36418572
36428571
36814752
36815724
36824175
37285146
37286415
38471625
41582736
41586372
42586137
42736815
42736851
42751863
42857136
42861357
46152837
46827135
46831752
47185263
47382516
47526138
47531682
48136275
48157263
48531726
51468273
51842736
51863724
52468317
52473861
52617483
52814736
53168247
53172864
53847162
57138642
57142863
57248136
57263148
57263184
57413862
58413627
58417263
61528374
62713584
62714853
63175824
63184275
63185247
63571428
63581427
63724815
63728514
63741825
64158273
64285713
64713528
64718253
68241753
71386425
72418536
72631485
73168524
73825164
74258136
74286135
75316824
82417536
82531746
83162574
84136275
>>*)

(*[[
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC _queens 16 0 0
!   if k = N then
LDLW 16
CONST 8
JNEQ L2
!     print(choice)
LDLW 20
CONST 0
GLOBAL _print
PCALL 1
JUMP L3
LABEL L2
!     y := 0;
CONST 0
STLW -4
!     while y < N do
JUMP L5
LABEL L4
!       j := 0; ok := true;
CONST 0
STLW -8
CONST 1
STLC -13
!       while ok and (j < k) do
JUMP L8
LABEL L7
! 	q := choice[j];
LDLW 20
LDLW -8
LDIW
STLW -12
! 	ok := (q <> y) and (q+j <> y+k) and (q-j <> y-k);
LDLW -12
LDLW -4
JEQ L12
LDLW -12
LDLW -8
PLUS
LDLW -4
LDLW 16
PLUS
JEQ L12
LDLW -12
LDLW -8
MINUS
LDLW -4
LDLW 16
MINUS
JEQ L12
CONST 1
JUMP L13
LABEL L12
CONST 0
LABEL L13
STLC -13
!         j := j+1
LDLW -8
CONST 1
PLUS
STLW -8
LABEL L8
LDLC -13
JNEQZ L10
JUMP L9
LABEL L10
LDLW -8
LDLW 16
JLT L7
LABEL L9
!       if ok then 
LDLC -13
JNEQZ L16
JUMP L18
LABEL L16
! 	choice[k] := y;
LDLW -4
LDLW 20
LDLW 16
STIW
! 	queens(k+1, choice)
LDLW 20
LDLW 16
CONST 1
PLUS
CONST 0
GLOBAL _queens
PCALL 2
LABEL L18
!       y := y+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L5
LDLW -4
CONST 8
JLT L4
LABEL L3
RETURN
END

PROC _print 4 0 0
!   x := 0;
CONST 0
STLW -4
!   while x < N do
JUMP L20
LABEL L19
!     print_num(choice[x]+1);
LDLW 16
LDLW -4
LDIW
CONST 1
PLUS
CONST 0
GLOBAL _print_num
PCALL 1
!     x := x+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L20
LDLW -4
CONST 8
JLT L19
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

PROC MAIN 0 0 0
!   queens(0, choice)
GLOBAL _choice
CONST 0
CONST 0
GLOBAL _queens
PCALL 2
RETURN
END

GLOVAR _choice 32
! End
]]*)
