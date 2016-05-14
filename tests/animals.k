Program (Main (MethDecl (main,-,=None,0,[],-1,[],Skip,NOP),NOP),
          [ClassDecl (Animal,
                       Object,
                       =Object,
                       [Variable(age,=Integer,=Field,4);
                         Variable(cuteness,=Integer,=Field,8)],
                       VTable ([MethDecl (isEqual,Boolean,=Object,1,[Formal 
                                                                    (that,
                                                                    Object)],0,[],Skip,NOP);
                                 MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                 MethDecl (init,Unit,=Animal,0,[],2,[],Seq 
                                            [Assign (Variable(age,=Integer,=Field,4),
                                                      Number (0,=Integer));
                                              Assign (Variable(cuteness,=Integer,=Field,8),
                                                       Number (0,=Integer));
                                              Skip],NOP);
                                 MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                            [Assign (Variable(age,=Integer,=Field,4),
                                                      Call (Variable(age,=Integer,=Field,4),
                                                             add,
                                                             [Number 
                                                               (1,=Integer)],
                                                             =Integer));
                                              Skip],NOP);
                                 MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                            [Return (Variable(age,=Integer,=Field,4));
                                              Skip],NOP);
                                 MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                            [Return (Variable(cuteness,=Integer,=Field,8));
                                              Skip],NOP);
                                 MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                            [Return (Call (Variable(cuteness,=Integer,=Field,8),
                                                            add,
                                                            [Call (Variable
                                                                    (a,=Animal,=Arg,20),
                                                                    cuteness,
                                                                    [],
                                                                    =Animal)],
                                                            =Integer));
                                              Skip],NOP)]));
            ClassDecl (Dog,
                        Animal,
                        =Animal,
                        [Variable(age,=Integer,=Field,4);
                          Variable(cuteness,=Integer,=Field,8);
                          Variable(x,=Integer,=Field,12)],
                        VTable ([MethDecl (isEqual,Boolean,=Object,1,
                                            [Formal (that, Object)],0,[],Skip,NOP);
                                  MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                  MethDecl (init,Unit,=Dog,0,[],2,[],Seq 
                                             [Assign (Variable(age,=Integer,=Field,4),
                                                       Number (1,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Number (8,=Integer));
                                               Assign (Variable(x,=Integer,=Field,12),
                                                        Number (1,=Integer));
                                               Skip],NOP);
                                  MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                             [Assign (Variable(age,=None,=None,-1),
                                                       Call (Variable
                                                              (age,=None,=None,-1),
                                                              add,
                                                              [Number 
                                                                (1,=None)],
                                                              =None));
                                               Skip],NOP);
                                  MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                             [Return (Variable(age,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                             [Return (Variable(cuteness,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                             [Return (Call (Variable(cuteness,=None,=None,-1),
                                                             add,
                                                             [Call (Variable
                                                                    (a,=None,=None,-1),
                                                                    cuteness,
                                                                    [],
                                                                    =None)],
                                                             =None));
                                               Skip],NOP)]));
            ClassDecl (Puppy,
                        Dog,
                        =Dog,
                        [Variable(age,=Integer,=Field,4);
                          Variable(cuteness,=Integer,=Field,8);
                          Variable(x,=Integer,=Field,12)],
                        VTable ([MethDecl (isEqual,Boolean,=Object,1,
                                            [Formal (that, Object)],0,[],Skip,NOP);
                                  MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                  MethDecl (init,Unit,=Puppy,0,[],2,[],Seq 
                                             [Assign (Variable(age,=Integer,=Field,4),
                                                       Number (0,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Number (1000,=Integer));
                                               Assign (Variable(x,=Integer,=Field,12),
                                                        Number (2,=Integer));
                                               Skip],NOP);
                                  MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                             [Assign (Variable(age,=None,=None,-1),
                                                       Call (Variable
                                                              (age,=None,=None,-1),
                                                              add,
                                                              [Number 
                                                                (1,=None)],
                                                              =None));
                                               Skip],NOP);
                                  MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                             [Return (Variable(age,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                             [Return (Variable(cuteness,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                             [Return (Call (Variable(cuteness,=None,=None,-1),
                                                             add,
                                                             [Call (Variable
                                                                    (a,=None,=None,-1),
                                                                    cuteness,
                                                                    [],
                                                                    =None)],
                                                             =None));
                                               Skip],NOP);
                                  MethDecl (ruinCarpet,Unit,=Puppy,0,[],7,
                                             [Variable(y,=Integer,=Local,-4)],Seq 
                                             [LocalVarDecl (y,
                                                             Integer,
                                                             =Integer);
                                               Assign (Variable(y,=Integer,=Local,-4),
                                                        Number (250,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Call (Variable
                                                               (cuteness,=Integer,=Field,8),
                                                               minus,
                                                               [Variable
                                                                 (y,=Integer,=Local,-4)],
                                                               =Integer));
                                               Skip],NOP)]))])
Program (Main (MethDecl (main,-,=None,0,[],-1,[],Skip,SEQ ...),SEQ ...),
          [ClassDecl (Animal,
                       Object,
                       =Object,
                       [Variable(age,=Integer,=Field,4);
                         Variable(cuteness,=Integer,=Field,8)],
                       VTable ([MethDecl (isEqual,Boolean,=Object,1,[Formal 
                                                                    (that,
                                                                    Object)],0,[],Skip,NOP);
                                 MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                 MethDecl (init,Unit,=Animal,0,[],2,[],Seq 
                                            [Assign (Variable(age,=Integer,=Field,4),
                                                      Number (0,=Integer));
                                              Assign (Variable(cuteness,=Integer,=Field,8),
                                                       Number (0,=Integer));
                                              Skip],SEQ ...);
                                 MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                            [Assign (Variable(age,=Integer,=Field,4),
                                                      Call (Variable(age,=Integer,=Field,4),
                                                             add,
                                                             [Number 
                                                               (1,=Integer)],
                                                             =Integer));
                                              Skip],SEQ ...);
                                 MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                            [Return (Variable(age,=Integer,=Field,4));
                                              Skip],SEQ ...);
                                 MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                            [Return (Variable(cuteness,=Integer,=Field,8));
                                              Skip],SEQ ...);
                                 MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                            [Return (Call (Variable(cuteness,=Integer,=Field,8),
                                                            add,
                                                            [Call (Variable
                                                                    (a,=Animal,=Arg,20),
                                                                    cuteness,
                                                                    [],
                                                                    =Animal)],
                                                            =Integer));
                                              Skip],SEQ ...)]));
            ClassDecl (Dog,
                        Animal,
                        =Animal,
                        [Variable(age,=Integer,=Field,4);
                          Variable(cuteness,=Integer,=Field,8);
                          Variable(x,=Integer,=Field,12)],
                        VTable ([MethDecl (isEqual,Boolean,=Object,1,
                                            [Formal (that, Object)],0,[],Skip,NOP);
                                  MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                  MethDecl (init,Unit,=Dog,0,[],2,[],Seq 
                                             [Assign (Variable(age,=Integer,=Field,4),
                                                       Number (1,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Number (8,=Integer));
                                               Assign (Variable(x,=Integer,=Field,12),
                                                        Number (1,=Integer));
                                               Skip],SEQ ...);
                                  MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                             [Assign (Variable(age,=None,=None,-1),
                                                       Call (Variable
                                                              (age,=None,=None,-1),
                                                              add,
                                                              [Number 
                                                                (1,=None)],
                                                              =None));
                                               Skip],NOP);
                                  MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                             [Return (Variable(age,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                             [Return (Variable(cuteness,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                             [Return (Call (Variable(cuteness,=None,=None,-1),
                                                             add,
                                                             [Call (Variable
                                                                    (a,=None,=None,-1),
                                                                    cuteness,
                                                                    [],
                                                                    =None)],
                                                             =None));
                                               Skip],NOP)]));
            ClassDecl (Puppy,
                        Dog,
                        =Dog,
                        [Variable(age,=Integer,=Field,4);
                          Variable(cuteness,=Integer,=Field,8);
                          Variable(x,=Integer,=Field,12)],
                        VTable ([MethDecl (isEqual,Boolean,=Object,1,
                                            [Formal (that, Object)],0,[],Skip,NOP);
                                  MethDecl (print,Unit,=Object,0,[],1,[],Skip,NOP);
                                  MethDecl (init,Unit,=Puppy,0,[],2,[],Seq 
                                             [Assign (Variable(age,=Integer,=Field,4),
                                                       Number (0,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Number (1000,=Integer));
                                               Assign (Variable(x,=Integer,=Field,12),
                                                        Number (2,=Integer));
                                               Skip],SEQ ...);
                                  MethDecl (haveBirthday,Unit,=Animal,0,[],3,[],Seq 
                                             [Assign (Variable(age,=None,=None,-1),
                                                       Call (Variable
                                                              (age,=None,=None,-1),
                                                              add,
                                                              [Number 
                                                                (1,=None)],
                                                              =None));
                                               Skip],NOP);
                                  MethDecl (age,Integer,=Animal,0,[],4,[],Seq 
                                             [Return (Variable(age,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (cuteness,Integer,=Animal,0,[],5,[],Seq 
                                             [Return (Variable(cuteness,=None,=None,-1));
                                               Skip],NOP);
                                  MethDecl (greet,Integer,=Animal,1,[Formal 
                                                                    (a,
                                                                    Animal)],6,[],Seq 
                                             [Return (Call (Variable(cuteness,=None,=None,-1),
                                                             add,
                                                             [Call (Variable
                                                                    (a,=None,=None,-1),
                                                                    cuteness,
                                                                    [],
                                                                    =None)],
                                                             =None));
                                               Skip],NOP);
                                  MethDecl (ruinCarpet,Unit,=Puppy,0,[],7,
                                             [Variable(y,=Integer,=Local,-4)],Seq 
                                             [LocalVarDecl (y,
                                                             Integer,
                                                             =Integer);
                                               Assign (Variable(y,=Integer,=Local,-4),
                                                        Number (250,=Integer));
                                               Assign (Variable(cuteness,=Integer,=Field,8),
                                                        Call (Variable
                                                               (cuteness,=Integer,=Field,8),
                                                               minus,
                                                               [Variable
                                                                 (y,=Integer,=Local,-4)],
                                                               =Integer));
                                               Skip],SEQ ...)]))])
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

PROC Animal.init 0 0 0
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
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
STOREW
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
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
STOREW
END

PROC Animal.haveBirthday 0 0 0
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
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
DUP 0
LOADW
CONST 8
PLUSA
LOADW
CONST 0
PCALLW 2
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
STOREW
END

PROC Animal.age 0 0 0
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
RETURNW
END

PROC Animal.cuteness 0 0 0
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
RETURNW
END

PROC Animal.greet 0 0 0
LOCAL 20
LOADW
DUP 0
LOADW
CONST 20
PLUSA
LOADW
CONST 0
PCALLW 1
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

DEFINE %Animal
WORD Object.isEqual
WORD Object.print
WORD Animal.init
WORD Animal.haveBirthday
WORD Animal.age
WORD Animal.cuteness
WORD Animal.greet

PROC Dog.init 0 0 0
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
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
STOREW
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 8
SWAP
CONST 4
PLUSA
STOREW
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
STOREW
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
LOCAL 16
LOADW
CONST 12
PLUSA
LOADW
STOREW
END

DEFINE %Dog
WORD Object.isEqual
WORD Object.print
WORD Dog.init
WORD Animal.haveBirthday
WORD Animal.age
WORD Animal.cuteness
WORD Animal.greet

PROC Puppy.init 0 0 0
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
LOCAL 16
LOADW
CONST 4
PLUSA
LOADW
STOREW
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 1000
SWAP
CONST 4
PLUSA
STOREW
LOCAL 16
LOADW
CONST 8
PLUSA
LOADW
STOREW
CONST 8
GLOBAL %Integer
CONST 0
GLOBAL _new
PCALLW 2
DUP 0
CONST 2
SWAP
CONST 4
PLUSA
STOREW
LOCAL 16
LOADW
CONST 12
PLUSA
LOADW
STOREW
END

PROC Puppy.ruinCarpet 4 0 0
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
CONST 250
SWAP
CONST 4
PLUSA
STOREW
LOCAL -4
LOADW
STOREW
LOCAL -4
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
END

DEFINE %Puppy
WORD Object.isEqual
WORD Object.print
WORD Puppy.init
WORD Animal.haveBirthday
WORD Animal.age
WORD Animal.cuteness
WORD Animal.greet
WORD Puppy.ruinCarpet

PROC MAIN 0 0 0
END

! END
