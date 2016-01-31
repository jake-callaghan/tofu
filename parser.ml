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
\011\000\011\000\012\000\012\000\013\000\013\000\013\000\014\000\
\014\000\015\000\015\000\015\000\015\000\016\000\016\000\017\000\
\017\000\000\000"

let yylen = "\002\000\
\006\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\002\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\005\000\003\000\002\000\007\000\011\000\007\000\
\002\000\001\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\001\000\001\000\004\000\002\000\002\000\003\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\017\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\003\000\019\000\000\000\000\000\
\000\000\036\000\000\000\000\000\015\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\000\000\039\000\000\000\000\000\000\000\004\000\007\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\023\000\014\000\013\000\000\000\000\000\
\000\000\008\000"

let yydgoto = "\002\000\
\004\000\061\000\041\000\042\000\070\000\071\000\086\000\062\000\
\092\000\093\000\015\000\065\000\024\000\025\000\026\000\058\000\
\066\000"

let yysindex = "\006\000\
\034\255\000\000\005\255\000\000\008\255\067\255\046\255\020\255\
\048\255\057\255\007\255\007\255\000\000\072\255\029\255\007\255\
\070\255\007\255\007\255\074\255\000\000\007\255\094\255\255\254\
\100\255\000\000\094\255\099\255\067\255\094\255\106\255\014\255\
\017\255\108\255\000\000\007\255\007\255\007\255\007\255\109\255\
\000\000\099\255\000\000\089\255\096\255\097\255\086\255\255\254\
\100\255\100\255\000\000\004\255\000\000\000\000\067\255\067\255\
\000\255\000\000\114\255\080\255\000\000\101\255\102\255\000\000\
\042\255\087\255\104\255\116\255\120\255\105\255\080\255\000\000\
\110\255\007\255\000\000\080\255\103\255\095\255\000\000\000\000\
\111\255\000\000\112\255\124\255\003\255\107\255\067\255\000\000\
\113\255\115\255\000\000\000\000\118\255\127\255\117\255\000\000\
\130\255\131\255\119\255\000\000\000\000\000\000\125\255\067\255\
\123\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\244\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\126\255\000\000\
\000\000\000\000\000\000\013\255\000\000\000\000\036\255\066\255\
\023\255\000\000\043\255\133\000\244\254\044\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\133\000\000\000\000\000\000\000\000\000\000\000\079\255\
\033\255\052\255\000\000\000\000\000\000\000\000\244\254\244\254\
\000\000\000\000\000\000\128\255\000\000\000\000\000\000\000\000\
\121\255\000\000\000\000\000\000\000\000\000\000\128\255\000\000\
\050\255\000\000\000\000\128\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\244\254\000\000\
\000\000\000\000\000\000\000\000\129\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\244\254\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\007\000\094\000\000\000\255\255\000\000\000\000\200\255\
\044\000\000\000\000\000\086\000\108\000\233\255\240\255\000\000\
\072\000"

let yytablesize = 152
let yytable = "\063\000\
\020\000\021\000\037\000\090\000\018\000\035\000\001\000\020\000\
\021\000\018\000\005\000\059\000\014\000\049\000\050\000\035\000\
\035\000\035\000\036\000\060\000\017\000\036\000\051\000\006\000\
\038\000\022\000\029\000\029\000\064\000\035\000\095\000\091\000\
\022\000\035\000\035\000\043\000\030\000\030\000\035\000\029\000\
\003\000\035\000\045\000\029\000\029\000\046\000\036\000\105\000\
\029\000\030\000\029\000\029\000\025\000\030\000\030\000\031\000\
\031\000\025\000\030\000\021\000\020\000\030\000\074\000\016\000\
\021\000\020\000\022\000\007\000\031\000\080\000\027\000\022\000\
\031\000\031\000\083\000\018\000\008\000\031\000\009\000\010\000\
\031\000\011\000\027\000\028\000\019\000\012\000\027\000\027\000\
\028\000\068\000\069\000\013\000\031\000\034\000\027\000\028\000\
\023\000\027\000\036\000\028\000\028\000\030\000\039\000\032\000\
\033\000\040\000\044\000\028\000\047\000\052\000\054\000\055\000\
\056\000\057\000\067\000\075\000\077\000\072\000\073\000\076\000\
\078\000\079\000\085\000\081\000\089\000\084\000\087\000\099\000\
\088\000\094\000\101\000\090\000\002\000\100\000\096\000\053\000\
\103\000\097\000\098\000\106\000\104\000\102\000\016\000\048\000\
\006\000\082\000\000\000\000\000\000\000\040\000\000\000\012\000"

let yycheck = "\056\000\
\001\001\002\001\004\001\001\001\017\001\022\000\001\000\001\001\
\002\001\022\001\006\001\008\001\006\000\037\000\038\000\003\001\
\004\001\005\001\005\001\016\001\001\001\005\001\039\000\016\001\
\026\001\026\001\004\001\005\001\029\001\017\001\087\000\029\001\
\026\001\021\001\022\001\029\000\004\001\005\001\026\001\017\001\
\007\001\029\001\029\001\021\001\022\001\029\001\005\001\104\000\
\026\001\017\001\022\001\029\001\017\001\021\001\022\001\004\001\
\005\001\022\001\026\001\017\001\017\001\029\001\021\001\018\001\
\022\001\022\001\017\001\001\001\017\001\071\000\005\001\022\001\
\021\001\022\001\076\000\028\001\010\001\026\001\012\001\013\001\
\029\001\015\001\017\001\005\001\028\001\019\001\021\001\022\001\
\017\001\010\001\011\001\025\001\023\001\020\001\029\001\017\001\
\011\000\012\000\005\001\021\001\022\001\016\000\003\001\018\000\
\019\000\007\001\001\001\029\001\001\001\001\001\022\001\016\001\
\016\001\028\001\001\001\029\001\001\001\017\001\017\001\016\001\
\001\001\017\001\028\001\014\001\001\001\023\001\016\001\001\001\
\017\001\023\001\001\001\001\001\000\000\017\001\022\001\042\000\
\018\001\023\001\021\001\017\001\016\001\098\000\017\001\036\000\
\017\001\074\000\255\255\255\255\255\255\029\001\255\255\023\001"

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
# 237 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
                        ( [] )
# 243 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 31 "parser.mly"
                                   ( _1 :: _2 )
# 251 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 34 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 259 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 35 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 268 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                        ( [] )
# 274 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 39 "parser.mly"
                                      ( _1 :: _2 )
# 282 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 42 "parser.mly"
                                                          ( MethDecl ((methodDesc _2 _5), _3, _8) )
# 292 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                              ( ClassVarDecl (variableDesc _2 _4) )
# 300 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                   ( [] )
# 306 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 47 "parser.mly"
                           ( _2 )
# 313 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 50 "parser.mly"
                ( [_1] )
# 320 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 51 "parser.mly"
                                 ( _1 :: _3 )
# 328 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 54 "parser.mly"
                         ( Formal (_1, _3) )
# 336 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
                   ( seq _1 )
# 343 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                  ( [_1] )
# 350 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                         ( _1 :: _3 )
# 358 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                   ( Skip )
# 364 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 65 "parser.mly"
                              ( LocalVarDecl (variableDesc _2 _4) )
# 372 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                        ( AssignStmt (_1, exprDesc _3) )
# 380 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                    ( ReturnStmt (exprDesc _2) )
# 387 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
                                         ( IfStmt (exprDesc _3, _6, Skip) )
# 395 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                                                ( IfStmt(exprDesc _3, _6, _10) )
# 404 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                            ( WhileStmt (exprDesc _3, _6) )
# 412 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                   ( PrintStmt (exprDesc _2) )
# 419 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                 ( NewLine )
# 425 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 75 "parser.mly"
                ( _1 )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 76 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 79 "parser.mly"
                ( _1 )
# 448 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 457 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 81 "parser.mly"
                         ( Binop (Minus, exprDesc _1, exprDesc _3) )
# 465 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 84 "parser.mly"
                 ( _1 )
# 472 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 85 "parser.mly"
                         ( Binop (_2, exprDesc _1, exprDesc _3) )
# 481 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 88 "parser.mly"
                ( _1 )
# 488 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
                ( Variable _1 )
# 495 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 90 "parser.mly"
                            ( Call (_1, _3, _4) )
# 504 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 91 "parser.mly"
                     ( Monop (Uminus, exprDesc _2))
# 511 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                     ( [] )
# 517 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 95 "parser.mly"
                                ( _2 )
# 524 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
               ( [_1] )
# 531 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 99 "parser.mly"
                           ( _1 :: _3 )
# 539 "parser.ml"
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
