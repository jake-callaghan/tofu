type token =
  | IDENT of (string)
  | NUMBER of (int)
  | TRUE
  | FALSE
  | THIS
  | MAIN
  | CLASS
  | EXTENDS
  | NEW
  | VAR
  | DEF
  | WHILE
  | IF
  | ELSE
  | LCURL
  | RCURL
  | ASSIGN
  | RETURN
  | DOT
  | COMMA
  | SEMI
  | COLON
  | EOF
  | BADTOK
  | NEWLINE
  | LBRAC
  | RBRAC

open Parsing;;
let _ = parse_error;;
# 18 "parser.mly"
	open Tree
	open Keiko
# 36 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* THIS *);
  262 (* MAIN *);
  263 (* CLASS *);
  264 (* EXTENDS *);
  265 (* NEW *);
  266 (* VAR *);
  267 (* DEF *);
  268 (* WHILE *);
  269 (* IF *);
  270 (* ELSE *);
  271 (* LCURL *);
  272 (* RCURL *);
  273 (* ASSIGN *);
  274 (* RETURN *);
  275 (* DOT *);
  276 (* COMMA *);
  277 (* SEMI *);
  278 (* COLON *);
    0 (* EOF *);
  279 (* BADTOK *);
  280 (* NEWLINE *);
  281 (* LBRAC *);
  282 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\005\000\005\000\006\000\
\006\000\007\000\007\000\008\000\008\000\009\000\002\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\005\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\001\000\004\000\002\000\001\000\001\000\001\000\001\000\001\000\
\002\000\004\000\002\000\002\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\040\000\000\000\000\000\029\000\030\000\
\031\000\028\000\000\000\000\000\000\000\000\000\000\000\025\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\001\000\000\000\017\000\000\000\000\000\037\000\
\019\000\000\000\000\000\000\000\000\000\003\000\000\000\039\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\004\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\009\000\000\000\011\000\000\000\000\000\023\000\014\000\
\013\000\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\017\000\043\000\044\000\067\000\068\000\080\000\086\000\
\087\000\018\000\019\000\020\000\023\000\036\000"

let yysindex = "\030\000\
\038\255\000\000\061\255\000\000\036\255\004\255\000\000\000\000\
\000\000\000\000\050\255\076\255\028\255\053\255\054\255\000\000\
\063\255\000\000\059\255\062\255\054\255\002\255\000\000\000\000\
\060\255\054\255\054\255\058\255\065\255\078\255\036\255\085\255\
\065\255\000\000\051\255\066\255\086\255\253\254\007\255\000\000\
\087\255\088\255\000\000\078\255\000\000\058\255\054\255\000\000\
\000\000\075\255\079\255\058\255\035\255\000\000\000\000\000\000\
\036\255\036\255\000\000\090\255\064\255\077\255\080\255\082\255\
\094\255\097\255\083\255\064\255\000\000\089\255\064\255\084\255\
\091\255\000\000\000\000\092\255\093\255\099\255\009\255\095\255\
\036\255\000\000\081\255\096\255\000\000\098\255\100\255\103\255\
\105\255\000\000\104\255\000\000\107\255\102\255\000\000\000\000\
\000\000\108\255\036\255\106\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\001\255\062\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\109\255\000\000\000\000\000\000\045\255\000\000\
\000\000\000\000\000\000\255\254\011\255\101\000\001\255\000\000\
\031\255\000\000\101\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\101\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\046\255\000\000\
\001\255\001\255\000\000\000\000\110\255\000\000\000\000\000\000\
\000\000\000\000\000\000\110\255\000\000\052\255\110\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\255\000\000\000\000\000\000\000\000\000\000\111\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\199\255\066\000\000\000\001\000\000\000\000\000\018\000\
\000\000\081\000\000\000\243\255\240\255\067\000"

let yytablesize = 137
let yytable = "\062\000\
\063\000\029\000\028\000\007\000\008\000\009\000\010\000\033\000\
\035\000\084\000\011\000\040\000\038\000\039\000\032\000\041\000\
\018\000\032\000\032\000\032\000\021\000\018\000\050\000\089\000\
\032\000\041\000\021\000\034\000\022\000\055\000\001\000\021\000\
\051\000\035\000\085\000\059\000\006\000\007\000\008\000\009\000\
\010\000\100\000\060\000\003\000\011\000\012\000\020\000\013\000\
\014\000\061\000\024\000\020\000\026\000\015\000\028\000\007\000\
\008\000\009\000\010\000\016\000\027\000\026\000\011\000\035\000\
\034\000\027\000\026\000\022\000\075\000\041\000\047\000\077\000\
\022\000\065\000\066\000\005\000\025\000\027\000\030\000\031\000\
\032\000\037\000\022\000\041\000\042\000\046\000\049\000\052\000\
\053\000\057\000\064\000\048\000\069\000\058\000\072\000\070\000\
\071\000\073\000\074\000\083\000\002\000\090\000\076\000\094\000\
\096\000\078\000\081\000\084\000\082\000\054\000\097\000\045\000\
\000\000\056\000\000\000\079\000\088\000\091\000\098\000\093\000\
\095\000\101\000\099\000\092\000\016\000\006\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000"

let yycheck = "\057\000\
\058\000\015\000\001\001\002\001\003\001\004\001\005\001\021\000\
\022\000\001\001\009\001\028\000\026\000\027\000\016\001\019\001\
\016\001\019\001\020\001\021\001\017\001\021\001\026\001\081\000\
\026\001\019\001\016\001\026\001\025\001\046\000\001\000\021\001\
\026\001\047\000\026\001\052\000\001\001\002\001\003\001\004\001\
\005\001\099\000\008\001\006\001\009\001\010\001\016\001\012\001\
\013\001\015\001\001\001\021\001\025\001\018\001\001\001\002\001\
\003\001\004\001\005\001\024\001\016\001\016\001\009\001\019\001\
\019\001\021\001\021\001\016\001\068\000\019\001\020\001\071\000\
\021\001\010\001\011\001\015\001\001\001\025\001\016\001\021\001\
\019\001\022\001\025\001\019\001\007\001\001\001\001\001\001\001\
\001\001\015\001\001\001\026\001\016\001\015\001\001\001\016\001\
\015\001\001\001\016\001\001\001\000\000\021\001\014\001\001\001\
\001\001\022\001\015\001\001\001\016\001\044\000\093\000\031\000\
\255\255\047\000\255\255\025\001\022\001\022\001\017\001\020\001\
\016\001\016\001\015\001\026\001\016\001\016\001\026\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\026\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  THIS\000\
  MAIN\000\
  CLASS\000\
  EXTENDS\000\
  NEW\000\
  VAR\000\
  DEF\000\
  WHILE\000\
  IF\000\
  ELSE\000\
  LCURL\000\
  RCURL\000\
  ASSIGN\000\
  RETURN\000\
  DOT\000\
  COMMA\000\
  SEMI\000\
  COLON\000\
  EOF\000\
  BADTOK\000\
  NEWLINE\000\
  LBRAC\000\
  RBRAC\000\
  "

let yynames_block = "\
  IDENT\000\
  NUMBER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 25 "parser.mly"
                                        ( Program ( mainDesc (methodDesc "main" "-" [] _3)  , _5 ) )
# 219 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                        ( [] )
# 225 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 31 "parser.mly"
                               ( _1 :: _2 )
# 233 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 34 "parser.mly"
                                                      ( ClassDecl (classDesc _2 "Object", _4) )
# 241 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 35 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 250 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                         ( [] )
# 256 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 39 "parser.mly"
                                  ( _1 :: _2 )
# 264 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 42 "parser.mly"
                                                          ( MethDecl (methodDesc _2 _5 _3 _8) )
# 274 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                                            ( InstanceVarDecl ((variableDesc _2),_4) )
# 282 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                        ( [] )
# 288 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 47 "parser.mly"
                                ( _2 )
# 295 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 50 "parser.mly"
                      ( [_1] )
# 302 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "parser.mly"
                                 ( _1 :: _3 )
# 310 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                            ( Formal (_1, _3) )
# 318 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 59 "parser.mly"
                     ( seq _1 )
# 325 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                    ( [_1] )
# 332 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 63 "parser.mly"
                         ( _1 :: _3 )
# 340 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "parser.mly"
                                          ( Skip )
# 346 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                                                 ( LocalVarDecl ((variableDesc _2),_4) )
# 354 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                                ( AssignStmt ((variableDesc _1), _3) )
# 362 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                                              ( ReturnStmt _2 )
# 369 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                                       ( IfStmt (_3, _6, Skip) )
# 377 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 71 "parser.mly"
                                                                   ( IfStmt(_3, _6, _10) )
# 386 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 72 "parser.mly"
                                                         ( WhileStmt (_3, _6) )
# 394 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                        ( Newline )
# 400 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 74 "parser.mly"
                                              ( UnitCall (_1, _3, _4) )
# 409 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 75 "parser.mly"
                                         ( UnitCall (exprDesc This, _1, _2))
# 417 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                 ( exprDesc This )
# 423 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parser.mly"
                  ( exprDesc (Number _1)  )
# 430 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                 ( exprDesc (Boolean true) )
# 436 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( exprDesc (Boolean false) )
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                   ( exprDesc (Variable (variableDesc _1)) )
# 449 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( exprDesc (NewObject _2) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 86 "parser.mly"
                         ( exprDesc (Call (_1, _3, _4)) )
# 465 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 87 "parser.mly"
                   ( exprDesc (Call (exprDesc This, _1, _2)))
# 473 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                        ( [] )
# 479 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 91 "parser.mly"
                                ( _2 )
# 486 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                   ( [_1] )
# 493 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 95 "parser.mly"
                           ( _1 :: _3 )
# 501 "parser.ml"
               : 'expr_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Tree.program)
