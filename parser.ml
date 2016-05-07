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
  | PRINT
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
  271 (* PRINT *);
  272 (* LCURL *);
  273 (* RCURL *);
  274 (* ASSIGN *);
  275 (* RETURN *);
  276 (* DOT *);
  277 (* COMMA *);
  278 (* SEMI *);
  279 (* COLON *);
    0 (* EOF *);
  280 (* BADTOK *);
  281 (* NEWLINE *);
  282 (* LBRAC *);
  283 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\005\000\005\000\006\000\
\006\000\007\000\007\000\008\000\008\000\009\000\002\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\013\000\013\000\014\000\014\000\
\000\000"

let yylen = "\002\000\
\005\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\004\000\001\000\004\000\002\000\001\000\001\000\001\000\001\000\
\001\000\002\000\004\000\002\000\002\000\003\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\030\000\031\000\
\032\000\029\000\000\000\000\000\000\000\000\000\000\000\000\000\
\026\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\000\000\001\000\000\000\017\000\
\000\000\000\000\038\000\019\000\000\000\000\000\025\000\000\000\
\000\000\003\000\000\000\040\000\000\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\000\000\000\000\004\000\007\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
\010\000\000\000\000\000\000\000\000\000\009\000\000\000\011\000\
\000\000\000\000\023\000\014\000\013\000\000\000\000\000\000\000\
\008\000"

let yydgoto = "\002\000\
\004\000\018\000\046\000\047\000\071\000\072\000\084\000\090\000\
\091\000\019\000\020\000\021\000\024\000\038\000"

let yysindex = "\022\000\
\027\255\000\000\023\255\000\000\043\255\248\254\000\000\000\000\
\000\000\000\000\040\255\078\255\025\255\054\255\057\255\062\255\
\000\000\067\255\000\000\063\255\066\255\062\255\002\255\000\000\
\000\000\064\255\062\255\062\255\062\255\065\255\068\255\082\255\
\043\255\089\255\068\255\000\000\029\255\069\255\091\255\005\255\
\007\255\008\255\000\000\092\255\093\255\000\000\082\255\000\000\
\065\255\062\255\000\000\000\000\079\255\081\255\000\000\065\255\
\014\255\000\000\000\000\000\000\043\255\043\255\000\000\097\255\
\050\255\083\255\084\255\086\255\098\255\102\255\087\255\050\255\
\000\000\094\255\050\255\088\255\080\255\000\000\000\000\096\255\
\090\255\104\255\016\255\095\255\043\255\000\000\099\255\100\255\
\000\000\101\255\103\255\108\255\105\255\000\000\109\255\000\000\
\112\255\107\255\000\000\000\000\000\000\110\255\043\255\113\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\053\255\111\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\037\255\
\000\000\000\000\000\000\000\000\000\000\255\254\056\255\114\000\
\053\255\000\000\059\255\000\000\106\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\114\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\255\000\000\053\255\053\255\000\000\000\000\
\117\255\000\000\000\000\000\000\000\000\000\000\000\000\117\255\
\000\000\060\255\117\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\053\255\000\000\000\000\000\000\
\000\000\000\000\114\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\195\255\068\000\000\000\221\255\000\000\000\000\019\000\
\000\000\084\000\000\000\242\255\238\255\069\000"

let yytablesize = 141
let yytable = "\066\000\
\067\000\031\000\030\000\007\000\008\000\009\000\010\000\035\000\
\037\000\022\000\011\000\043\000\040\000\041\000\042\000\033\000\
\088\000\023\000\033\000\033\000\033\000\064\000\001\000\093\000\
\044\000\033\000\044\000\044\000\036\000\065\000\059\000\053\000\
\003\000\054\000\055\000\037\000\079\000\063\000\005\000\081\000\
\025\000\104\000\089\000\006\000\007\000\008\000\009\000\010\000\
\044\000\050\000\027\000\011\000\012\000\028\000\013\000\014\000\
\036\000\015\000\028\000\069\000\070\000\016\000\030\000\007\000\
\008\000\009\000\010\000\017\000\027\000\018\000\011\000\035\000\
\021\000\027\000\018\000\020\000\022\000\021\000\026\000\028\000\
\020\000\022\000\029\000\032\000\033\000\034\000\039\000\044\000\
\045\000\049\000\023\000\052\000\056\000\057\000\061\000\051\000\
\062\000\068\000\076\000\073\000\074\000\075\000\077\000\078\000\
\087\000\083\000\086\000\080\000\098\000\100\000\082\000\085\000\
\088\000\002\000\058\000\101\000\048\000\092\000\060\000\000\000\
\094\000\099\000\095\000\097\000\102\000\103\000\000\000\096\000\
\000\000\105\000\033\000\016\000\039\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\061\000\
\062\000\016\000\001\001\002\001\003\001\004\001\005\001\022\000\
\023\000\018\001\009\001\030\000\027\000\028\000\029\000\017\001\
\001\001\026\001\020\001\021\001\022\001\008\001\001\000\085\000\
\020\001\027\001\020\001\020\001\027\001\016\001\049\000\027\001\
\006\001\027\001\027\001\050\000\072\000\056\000\016\001\075\000\
\001\001\103\000\027\001\001\001\002\001\003\001\004\001\005\001\
\020\001\021\001\026\001\009\001\010\001\017\001\012\001\013\001\
\020\001\015\001\022\001\010\001\011\001\019\001\001\001\002\001\
\003\001\004\001\005\001\025\001\017\001\017\001\009\001\020\001\
\017\001\022\001\022\001\017\001\017\001\022\001\001\001\026\001\
\022\001\022\001\026\001\017\001\022\001\020\001\023\001\020\001\
\007\001\001\001\026\001\001\001\001\001\001\001\016\001\027\001\
\016\001\001\001\001\001\017\001\017\001\016\001\001\001\017\001\
\001\001\026\001\017\001\014\001\001\001\001\001\023\001\016\001\
\001\001\000\000\047\000\097\000\033\000\023\001\050\000\255\255\
\022\001\017\001\023\001\021\001\018\001\016\001\255\255\027\001\
\255\255\017\001\020\001\017\001\027\001\017\001\255\255\255\255\
\255\255\255\255\255\255\255\255\027\001"

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
  PRINT\000\
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
# 24 "parser.mly"
                                        ( Program (MainBody(_3), _5) )
# 226 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                        ( [] )
# 232 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 30 "parser.mly"
                               ( _1 :: _2 )
# 240 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                                      ( ClassDecl (classDesc _2 "Object", _4) )
# 248 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 34 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 257 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                         ( [] )
# 263 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 38 "parser.mly"
                                  ( _1 :: _2 )
# 271 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "parser.mly"
                                                          ( MethDecl (methodDesc _2 _5 _3 _8) )
# 281 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                                            ( InstanceVarDecl ((variableDesc _2),_4) )
# 289 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                        ( [] )
# 295 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 46 "parser.mly"
                                ( _2 )
# 302 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 49 "parser.mly"
                      ( [_1] )
# 309 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                                 ( _1 :: _3 )
# 317 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                            ( Formal (_1, _3) )
# 325 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 58 "parser.mly"
                     ( seq _1 )
# 332 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                    ( [_1] )
# 339 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 62 "parser.mly"
                         ( _1 :: _3 )
# 347 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                                          ( Skip )
# 353 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                                                 ( LocalVarDecl ((variableDesc _2),_4) )
# 361 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                                ( AssignStmt (_1, _3) )
# 369 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                                              ( ReturnStmt _2 )
# 376 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                                       ( IfStmt (_3, _6, Skip) )
# 384 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                                                   ( IfStmt(_3, _6, _10) )
# 393 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 71 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 401 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                                                 ( PrintStmt (_3) )
# 408 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                                        ( Newline )
# 414 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 74 "parser.mly"
                                              ( UnitCall (_1, _3, _4) )
# 423 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 75 "parser.mly"
                                         ( UnitCall (exprDesc This, _1, _2))
# 431 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                 ( exprDesc This )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "parser.mly"
                  ( exprDesc (Number _1)  )
# 444 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                 ( exprDesc (Boolean true) )
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                  ( exprDesc (Boolean false) )
# 456 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
                   ( exprDesc (Variable (variableDesc _1)) )
# 463 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 85 "parser.mly"
                   ( exprDesc (NewObject _2) )
# 470 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 86 "parser.mly"
                         ( exprDesc (Call (_1, _3, _4)) )
# 479 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 87 "parser.mly"
                   ( exprDesc (Call (exprDesc This, _1, _2)))
# 487 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                        ( [] )
# 493 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 91 "parser.mly"
                                ( _2 )
# 500 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                   ( [_1] )
# 507 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 95 "parser.mly"
                           ( _1 :: _3 )
# 515 "parser.ml"
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
