Program (Main (MethDecl (main,-,=None,0,[],-1,[Variable(x,=Integer,=None,-4)],Seq 
                          [LocalVarDecl (x, Integer, =Integer);
                            Assign (Variable(x,=Integer,=None,-4),
                                     Number (15,=Integer));
                            Call (Call (Variable(x,=Integer,=None,-4),
                                         add,
                                         [Variable(x,=Integer,=None,-4)],
                                         =Integer),
                                   print,
                                   []);
                            Skip],NOP),NOP),
          [ClassDecl (Box,
                       Integer,
                       =Integer,
                       [Variable(_value,=PRIMITIVE,=Field,4);
                         Variable(value,=Integer,=Field,8)],
                       VTable ([MethDecl (isEqual,Boolean,=Integer,1,
                                           [Formal (that, Object)],0,[],Skip,NOP);
                                 MethDecl (print,Unit,=Integer,0,[],1,[],Skip,NOP);
                                 MethDecl (add,Integer,=Box,1,[Formal 
                                                                (int,
                                                                  Integer)],2,[],Seq 
                                            [Return (Call (Variable(value,=Integer,=Field,8),
                                                            add,
                                                            [Variable
                                                              (int,=Integer,=Arg,20)],
                                                            =Integer));
                                              Skip],NOP);
                                 MethDecl (minus,Integer,=Box,1,[Formal 
                                                                  (int,
                                                                    Integer)],3,[],Seq 
                                            [Assign (Variable(value,=Integer,=Field,8),
                                                      Call (Variable(value,=Integer,=Field,8),
                                                             minus,
                                                             [Variable
                                                               (int,=Integer,=Arg,20)],
                                                             =Integer));
                                              Return (Variable(value,=Integer,=Field,8));
                                              Skip],NOP);
                                 MethDecl (multiply,Integer,=Integer,1,
                                            [Formal (that, Integer)],4,[],Skip,NOP);
                                 MethDecl (divide,Integer,=Integer,1,
                                            [Formal (that, Integer)],5,[],Skip,NOP);
                                 MethDecl (isLessThan,Boolean,=Integer,1,
                                            [Formal (that, Integer)],6,[],Skip,NOP);
                                 MethDecl (isLessThanOrEqual,Boolean,=Integer,1,
                                            [Formal (that, Integer)],7,[],Skip,NOP);
                                 MethDecl (isGreaterThan,Boolean,=Integer,1,
                                            [Formal (that, Integer)],8,[],Skip,NOP);
                                 MethDecl (isGreaterThanOrEqual,Boolean,=Integer,1,
                                            [Formal (that, Integer)],9,[],Skip,NOP)]))])
Program (Main (MethDecl (main,-,=None,0,[],-1,[Variable(x,=Integer,=None,-4)],Seq 
                          [LocalVarDecl (x, Integer, =Integer);
                            Assign (Variable(x,=Integer,=None,-4),
                                     Number (15,=Integer));
                            Call (Call (Variable(x,=Integer,=None,-4),
                                         add,
                                         [Variable(x,=Integer,=None,-4)],
                                         =Integer),
                                   print,
                                   []);
                            Skip],SEQ ...),SEQ ...),
          [ClassDecl (Box,
                       Integer,
                       =Integer,
                       [Variable(_value,=PRIMITIVE,=Field,4);
                         Variable(value,=Integer,=Field,8)],
                       VTable ([MethDecl (isEqual,Boolean,=Integer,1,
                                           [Formal (that, Object)],0,[],Skip,NOP);
                                 MethDecl (print,Unit,=Integer,0,[],1,[],Skip,NOP);
                                 MethDecl (add,Integer,=Box,1,[Formal 
                                                                (int,
                                                                  Integer)],2,[],Seq 
                                            [Return (Call (Variable(value,=Integer,=Field,8),
                                                            add,
                                                            [Variable
                                                              (int,=Integer,=Arg,20)],
                                                            =Integer));
                                              Skip],SEQ ...);
                                 MethDecl (minus,Integer,=Box,1,[Formal 
                                                                  (int,
                                                                    Integer)],3,[],Seq 
                                            [Assign (Variable(value,=Integer,=Field,8),
                                                      Call (Variable(value,=Integer,=Field,8),
                                                             minus,
                                                             [Variable
                                                               (int,=Integer,=Arg,20)],
                                                             =Integer));
                                              Return (Variable(value,=Integer,=Field,8));
                                              Skip],SEQ ...);
                                 MethDecl (multiply,Integer,=Integer,1,
                                            [Formal (that, Integer)],4,[],Skip,NOP);
                                 MethDecl (divide,Integer,=Integer,1,
                                            [Formal (that, Integer)],5,[],Skip,NOP);
                                 MethDecl (isLessThan,Boolean,=Integer,1,
                                            [Formal (that, Integer)],6,[],Skip,NOP);
                                 MethDecl (isLessThanOrEqual,Boolean,=Integer,1,
                                            [Formal (that, Integer)],7,[],Skip,NOP);
                                 MethDecl (isGreaterThan,Boolean,=Integer,1,
                                            [Formal (that, Integer)],8,[],Skip,NOP);
                                 MethDecl (isGreaterThanOrEqual,Boolean,=Integer,1,
                                            [Formal (that, Integer)],9,[],Skip,NOP)]))])
MODULE Main 0 0
IMPORT Lib 0
ENDHDR

PROC Object.isEqual 0 0 0
LOCAL 20
LOADW
LOCAL 16
LOADW
JEQ L1
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L2
LABEL L1
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L2
RETURNW
END

PROC Object.print 0 0 0
LOCAL 16
LOADW
CONST 0
GLOBAL _print_num
PCALL 1
END

DEFINE %Object
WORD Object.isEqual
WORD Object.print

PROC Integer.isEqual 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JEQ L3
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L4
LABEL L3
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L4
RETURNW
END

PROC Integer.print 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
CONST 0
GLOBAL _print_num
PCALL 1
END

PROC Integer.add 0 0 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
PLUS
SWAP
CONST 4
PLUSA
STOREW
RETURNW
END

PROC Integer.minus 0 0 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
MINUS
SWAP
CONST 4
PLUSA
STOREW
RETURNW
END

PROC Integer.multiply 0 0 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
TIMES
SWAP
CONST 4
PLUSA
STOREW
RETURNW
END

PROC Integer.divide 0 0 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
DIV
SWAP
CONST 4
PLUSA
STOREW
RETURNW
END

PROC Integer.isLessThan 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JLT L5
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L6
LABEL L5
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L6
RETURNW
END

PROC Integer.isLessThanOrEqual 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JLEQ L7
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L8
LABEL L7
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L8
RETURNW
END

PROC Integer.isGreaterThan 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JGT L9
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L10
LABEL L9
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L10
RETURNW
END

PROC Integer.isGreaterThanOrEqual 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JGT L11
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L12
LABEL L11
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L12
RETURNW
END

DEFINE %Integer
WORD Integer.isEqual
WORD Integer.print
WORD Integer.add
WORD Integer.minus
WORD Integer.multiply
WORD Integer.divide
WORD Integer.isLessThan
WORD Integer.isLessThanOrEqual
WORD Integer.isGreaterThan
WORD Integer.isGreaterThanOrEqual

PROC Boolean.isEqual 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
JEQ L13
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L14
LABEL L13
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L14
RETURNW
END

PROC Boolean.print 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
CONST 0
GLOBAL _print_num
PCALL 1
END

PROC Boolean.not 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
CONST 0
JEQ L15
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L16
LABEL L15
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L16
RETURNW
END

PROC Boolean.and 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
TIMES
CONST 0
JEQ L17
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L18
LABEL L17
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L18
RETURNW
END

PROC Boolean.or 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
LOCAL 20
LOADW
CONST 4
PLUSA
LOADW
CONST 4
PLUSA
LOADW
PLUS
CONST 0
JEQ L19
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
JUMP L20
LABEL L19
CONST 8
GLOBAL %Boolean
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 0
SWAP
CONST 4
PLUSA
STOREW
SWAP
CONST 4
PLUSA
STOREW
LABEL L20
RETURNW
END

DEFINE %Boolean
WORD Boolean.isEqual
WORD Boolean.print
WORD Boolean.not
WORD Boolean.and
WORD Boolean.or

PROC Box.add 0 0 0
LOCAL 20
LOADW
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
DUP 0
LOADW
CONST 8
PLUSA
LOADW
CONST 0
PCALLW 2
RETURNW
END

PROC Box.minus 0 0 0
LOCAL 20
LOADW
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
DUP 0
LOADW
CONST 12
PLUSA
LOADW
CONST 0
PCALLW 2
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
STOREW
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
RETURNW
END

DEFINE %Box
WORD Integer.isEqual
WORD Integer.print
WORD Box.add
WORD Box.minus
WORD Integer.multiply
WORD Integer.divide
WORD Integer.isLessThan
WORD Integer.isLessThanOrEqual
WORD Integer.isGreaterThan
WORD Integer.isGreaterThanOrEqual

PROC MAIN 0 0 0
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
LOCAL -4
STOREW
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 15
SWAP
CONST 4
PLUSA
STOREW
GLOBAL _x
STOREW
GLOBAL _x
GLOBAL _x
DUP 0
LOADW
CONST 8
PLUSA
LOADW
CONST 0
PCALLW 2
DUP 0
LOADW
CONST 4
PLUSA
LOADW
CONST 0
PCALL 1
END

GLOVAR _x 4
! END
