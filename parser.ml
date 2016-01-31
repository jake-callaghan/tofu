type token =
  | IDENT of (string)
  | NUMBER of (int)
  | MULOP of (Keiko.op)
  | ADDOP of (Keiko.op)
  | RELOP of (Keiko.op)
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
  | MINUS
  | EQUALS
  | LBRAC
  | RBRAC

open Parsing;;
let _ = parse_error;;
# 20 "parser.mly"
	open Keiko
	open Tree
# 39 "parser.ml"
let yytransl_const = [|
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
  282 (* MINUS *);
  283 (* EQUALS *);
  284 (* LBRAC *);
  285 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
  259 (* MULOP *);
  260 (* ADDOP *);
  261 (* RELOP *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\005\000\005\000\006\000\
\006\000\007\000\007\000\009\000\009\000\010\000\008\000\002\000\
\002\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\012\000\012\000\013\000\013\000\013\000\014\000\014\000\
\015\000\015\000\015\000\015\000\016\000\016\000\017\000\017\000\
\000\000"

let yylen = "\002\000\
\006\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\002\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\003\000\002\000\007\000\011\000\007\000\002\000\
\001\000\001\000\003\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\004\000\002\000\002\000\003\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\017\000\
\000\000\000\000\000\000\000\000\000\000\000\000\032\000\000\000\
\003\000\000\000\000\000\000\000\035\000\000\000\000\000\015\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\000\000\038\000\000\000\000\000\
\000\000\004\000\007\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\010\000\011\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\022\000\014\000\
\013\000\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\056\000\038\000\039\000\065\000\066\000\081\000\057\000\
\087\000\088\000\014\000\060\000\022\000\023\000\024\000\053\000\
\061\000"

let yysindex = "\012\000\
\034\255\000\000\004\255\000\000\008\255\064\255\029\255\248\254\
\032\255\007\255\007\255\000\000\047\255\062\255\007\255\007\255\
\007\255\072\255\000\000\007\255\095\255\255\254\098\255\000\000\
\095\255\099\255\064\255\095\255\014\255\017\255\101\255\000\000\
\007\255\007\255\007\255\007\255\104\255\000\000\099\255\000\000\
\091\255\092\255\082\255\255\254\098\255\098\255\000\000\254\254\
\000\000\064\255\064\255\000\255\000\000\108\255\001\255\000\000\
\094\255\096\255\000\000\002\255\083\255\100\255\113\255\114\255\
\102\255\001\255\000\000\103\255\007\255\000\000\001\255\097\255\
\090\255\000\000\000\000\105\255\000\000\106\255\121\255\003\255\
\107\255\064\255\000\000\109\255\110\255\000\000\000\000\111\255\
\123\255\112\255\000\000\124\255\125\255\116\255\000\000\000\000\
\000\000\119\255\064\255\120\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\036\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\122\255\000\000\000\000\
\000\000\013\255\000\000\000\000\044\255\046\255\023\255\000\000\
\063\255\127\000\036\255\071\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\127\000\000\000\
\000\000\000\000\000\000\065\255\033\255\052\255\000\000\000\000\
\000\000\036\255\036\255\000\000\000\000\000\000\126\255\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\000\000\126\255\000\000\081\255\000\000\000\000\126\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\255\000\000\000\000\000\000\000\000\000\000\117\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\009\000\089\000\000\000\033\000\000\000\000\000\205\255\
\043\000\000\000\000\000\080\000\105\000\037\000\241\255\000\000\
\072\000"

let yytablesize = 144
let yytable = "\058\000\
\018\000\019\000\034\000\085\000\032\000\054\000\033\000\018\000\
\019\000\005\000\063\000\064\000\001\000\055\000\013\000\034\000\
\034\000\034\000\033\000\016\000\047\000\033\000\069\000\006\000\
\035\000\020\000\028\000\028\000\059\000\034\000\090\000\086\000\
\020\000\034\000\034\000\040\000\029\000\029\000\034\000\028\000\
\003\000\034\000\041\000\028\000\028\000\042\000\015\000\100\000\
\028\000\029\000\026\000\028\000\018\000\029\000\029\000\030\000\
\030\000\018\000\029\000\017\000\024\000\029\000\026\000\026\000\
\007\000\024\000\026\000\026\000\030\000\027\000\045\000\046\000\
\030\000\030\000\026\000\008\000\009\000\030\000\010\000\020\000\
\030\000\027\000\011\000\027\000\020\000\027\000\027\000\019\000\
\012\000\021\000\025\000\031\000\019\000\027\000\028\000\029\000\
\030\000\021\000\075\000\033\000\036\000\043\000\021\000\078\000\
\048\000\037\000\050\000\051\000\062\000\052\000\067\000\070\000\
\068\000\072\000\073\000\071\000\076\000\080\000\074\000\079\000\
\082\000\084\000\083\000\094\000\096\000\085\000\002\000\049\000\
\095\000\089\000\091\000\093\000\092\000\098\000\099\000\097\000\
\101\000\044\000\016\000\012\000\077\000\000\000\006\000\039\000"

let yycheck = "\051\000\
\001\001\002\001\004\001\001\001\020\000\008\001\005\001\001\001\
\002\001\006\001\010\001\011\001\001\000\016\001\006\000\003\001\
\004\001\005\001\005\001\028\001\036\000\005\001\021\001\016\001\
\026\001\026\001\004\001\005\001\029\001\017\001\082\000\029\001\
\026\001\021\001\022\001\027\000\004\001\005\001\026\001\017\001\
\007\001\029\001\029\001\021\001\022\001\029\001\018\001\099\000\
\026\001\017\001\005\001\029\001\017\001\021\001\022\001\004\001\
\005\001\022\001\026\001\028\001\017\001\029\001\017\001\017\001\
\001\001\022\001\021\001\022\001\017\001\005\001\034\000\035\000\
\021\001\022\001\029\001\012\001\013\001\026\001\015\001\017\001\
\029\001\017\001\019\001\022\001\022\001\021\001\022\001\017\001\
\025\001\010\000\011\000\020\001\022\001\029\001\015\000\016\000\
\017\000\017\001\066\000\005\001\003\001\001\001\022\001\071\000\
\001\001\007\001\016\001\016\001\001\001\028\001\017\001\029\001\
\017\001\001\001\001\001\016\001\014\001\028\001\017\001\023\001\
\016\001\001\001\017\001\001\001\001\001\001\001\000\000\039\000\
\017\001\023\001\022\001\021\001\023\001\018\001\016\001\093\000\
\017\001\033\000\017\001\023\001\069\000\255\255\017\001\029\001"

let yynames_const = "\
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
  MINUS\000\
  EQUALS\000\
  LBRAC\000\
  RBRAC\000\
  "

let yynames_block = "\
  IDENT\000\
  NUMBER\000\
  MULOP\000\
  ADDOP\000\
  RELOP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 27 "parser.mly"
                                                  ( Program (MainDecl(_4), _6) )
# 232 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                        ( [] )
# 238 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 31 "parser.mly"
                                   ( _1 :: _2 )
# 246 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 34 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 254 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 35 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 263 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                        ( [] )
# 269 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 39 "parser.mly"
                                      ( _1 :: _2 )
# 277 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 42 "parser.mly"
                                                          ( MethDecl ((methDesc _2 _5), _3, _8) )
# 287 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                              ( VarDecl( varDesc _2 _4 ) )
# 295 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                       ( [] )
# 301 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 47 "parser.mly"
                           ( _2 )
# 308 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 50 "parser.mly"
                ( [_1] )
# 315 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "parser.mly"
                                 ( _1 :: _3 )
# 323 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                         ( Formal (_1, _3) )
# 331 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
                   ( seq _1 )
# 338 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                  ( [_1] )
# 345 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                         ( _1 :: _3 )
# 353 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                   ( Skip )
# 359 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                        ( AssignStmt (_1, exprDesc _3) )
# 367 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                    ( ReturnStmt (exprDesc _2) )
# 374 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 67 "parser.mly"
                                         ( IfStmt (exprDesc _3, _6, Skip) )
# 382 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
                                                                ( IfStmt(exprDesc _3, _6, _10) )
# 391 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                            ( WhileStmt (exprDesc _3, _6) )
# 399 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                   ( PrintStmt (exprDesc _2) )
# 406 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                 ( NewLine )
# 412 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 74 "parser.mly"
                ( _1 )
# 419 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 75 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 428 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 78 "parser.mly"
                ( _1 )
# 435 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 444 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
                         ( Binop (Minus, exprDesc _1, exprDesc _3) )
# 452 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 83 "parser.mly"
                 ( _1 )
# 459 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 84 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 468 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "parser.mly"
                ( _1 )
# 475 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                ( Variable _1 )
# 482 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 89 "parser.mly"
                            ( Call (_1, _3, _4) )
# 491 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 90 "parser.mly"
                     ( Monop (Uminus, exprDesc _2))
# 498 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                     ( [] )
# 504 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 94 "parser.mly"
                                ( _2 )
# 511 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
               ( [_1] )
# 518 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 98 "parser.mly"
                           ( _1 :: _3 )
# 526 "parser.ml"
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
