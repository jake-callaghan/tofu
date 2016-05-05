(* N queens with bitmap arrays *)

const N = 8;

var 
  choice: array N of integer;
  rank: array N of boolean;
  diagup, diagdown: array 2 * N - 1 of boolean;

proc queens(k: integer);
  var y, j, q: integer; ok: boolean;
begin
  if k = N then
    print()
  else
    y := 0;
    while y < N do
      if rank[y] and diagup[k+y] and diagdown[k-y+N-1] then
	rank[y] := false; diagup[k+y] := false; diagdown[k-y+N-1] := false;
	choice[k] := y; queens(k+1);
	rank[y] := true; diagup[k+y] := true; diagdown[k-y+N-1] := true;
      end;
      y := y+1
    end
  end
end;

proc print();
  var x: integer;
begin
  x := 0;
  while x < N do
    print_num(choice[x]+1);
    x := x+1
  end;
  newline()
end;

proc init();
  var i: integer;
begin
  i := 0; 
  while i < N do 
    rank[i] := true; 
    i := i+1 
  end;
  i := 0; 
  while i < 2*N-1 do 
    diagup[i] := true; diagdown[i] := true ;
    i := i+1
  end
end;

begin
  init();
  queens(0)
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
!     print()
CONST 0
GLOBAL _print
PCALL 0
JUMP L3
LABEL L2
!     y := 0;
CONST 0
STLW -4
!     while y < N do
JUMP L5
LABEL L4
!       if rank[y] and diagup[k+y] and diagdown[k-y+N-1] then
GLOBAL _rank
LDLW -4
LDIC
JNEQZ L11
JUMP L9
LABEL L11
GLOBAL _diagup
LDLW 16
LDLW -4
PLUS
LDIC
JNEQZ L10
JUMP L9
LABEL L10
GLOBAL _diagdown
LDLW 16
LDLW -4
MINUS
CONST 8
PLUS
CONST 1
MINUS
LDIC
JNEQZ L7
JUMP L9
LABEL L7
! 	rank[y] := false; diagup[k+y] := false; diagdown[k-y+N-1] := false;
CONST 0
GLOBAL _rank
LDLW -4
STIC
CONST 0
GLOBAL _diagup
LDLW 16
LDLW -4
PLUS
STIC
CONST 0
GLOBAL _diagdown
LDLW 16
LDLW -4
MINUS
CONST 8
PLUS
CONST 1
MINUS
STIC
! 	choice[k] := y; queens(k+1);
LDLW -4
GLOBAL _choice
LDLW 16
STIW
LDLW 16
CONST 1
PLUS
CONST 0
GLOBAL _queens
PCALL 1
! 	rank[y] := true; diagup[k+y] := true; diagdown[k-y+N-1] := true;
CONST 1
GLOBAL _rank
LDLW -4
STIC
CONST 1
GLOBAL _diagup
LDLW 16
LDLW -4
PLUS
STIC
CONST 1
GLOBAL _diagdown
LDLW 16
LDLW -4
MINUS
CONST 8
PLUS
CONST 1
MINUS
STIC
LABEL L9
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
JUMP L13
LABEL L12
!     print_num(choice[x]+1);
GLOBAL _choice
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
LABEL L13
LDLW -4
CONST 8
JLT L12
!   newline()
CONST 0
GLOBAL _newline
PCALL 0
RETURN
END

PROC _init 4 0 0
!   i := 0; 
CONST 0
STLW -4
!   while i < N do 
JUMP L16
LABEL L15
!     rank[i] := true; 
CONST 1
GLOBAL _rank
LDLW -4
STIC
!     i := i+1 
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L16
LDLW -4
CONST 8
JLT L15
!   i := 0; 
CONST 0
STLW -4
!   while i < 2*N-1 do 
JUMP L19
LABEL L18
!     diagup[i] := true; diagdown[i] := true ;
CONST 1
GLOBAL _diagup
LDLW -4
STIC
CONST 1
GLOBAL _diagdown
LDLW -4
STIC
!     i := i+1
LDLW -4
CONST 1
PLUS
STLW -4
LABEL L19
LDLW -4
CONST 15
JLT L18
RETURN
END

PROC MAIN 0 0 0
!   init();
CONST 0
GLOBAL _init
PCALL 0
!   queens(0)
CONST 0
CONST 0
GLOBAL _queens
PCALL 1
RETURN
END

GLOVAR _choice 32
GLOVAR _rank 8
GLOVAR _diagup 15
GLOVAR _diagdown 15
! End
]]*)
