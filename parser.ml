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
\006\000\007\000\007\000\008\000\008\000\009\000\002\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
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
\000\000\000\000\000\000\000\000\026\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\033\000\000\000\003\000\019\000\000\000\
\000\000\000\000\036\000\000\000\000\000\000\000\000\000\038\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\000\000\000\000\039\000\000\000\000\000\000\000\004\000\007\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\009\000\
\000\000\000\000\000\000\023\000\014\000\013\000\000\000\000\000\
\000\000\008\000"

let yydgoto = "\002\000\
\004\000\014\000\042\000\043\000\070\000\071\000\086\000\092\000\
\093\000\015\000\016\000\065\000\025\000\026\000\027\000\059\000\
\066\000"

let yysindex = "\004\000\
\000\255\000\000\020\255\000\000\249\254\068\255\052\255\072\255\
\051\255\054\255\016\255\016\255\000\000\071\255\000\000\063\255\
\016\255\066\255\016\255\016\255\074\255\000\000\016\255\087\255\
\019\255\092\255\000\000\087\255\090\255\068\255\087\255\098\255\
\008\255\015\255\101\255\000\000\016\255\016\255\016\255\016\255\
\104\255\000\000\090\255\000\000\084\255\091\255\094\255\085\255\
\019\255\092\255\092\255\000\000\254\254\000\000\000\000\068\255\
\068\255\001\255\000\000\113\255\005\255\099\255\100\255\000\000\
\014\255\086\255\102\255\118\255\119\255\105\255\005\255\000\000\
\107\255\016\255\000\000\005\255\103\255\095\255\000\000\000\000\
\108\255\000\000\110\255\124\255\003\255\106\255\068\255\000\000\
\109\255\111\255\000\000\000\000\112\255\127\255\115\255\000\000\
\129\255\134\255\120\255\000\000\000\000\000\000\121\255\068\255\
\122\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\030\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\123\255\
\000\000\000\000\000\000\000\000\007\255\000\000\000\000\032\255\
\069\255\017\255\000\000\038\255\136\000\030\255\039\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\136\000\000\000\000\000\000\000\000\000\000\000\
\079\255\036\255\046\255\000\000\000\000\000\000\000\000\030\255\
\030\255\000\000\000\000\000\000\125\255\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\000\000\000\000\125\255\000\000\
\042\255\000\000\000\000\125\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\030\255\000\000\
\000\000\000\000\000\000\000\000\126\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\255\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\200\255\098\000\000\000\251\255\000\000\000\000\046\000\
\000\000\115\000\000\000\092\000\109\000\038\000\241\255\000\000\
\073\000"

let yytablesize = 149
let yytable = "\062\000\
\063\000\021\000\022\000\090\000\001\000\060\000\003\000\036\000\
\006\000\035\000\035\000\035\000\037\000\061\000\068\000\069\000\
\021\000\022\000\037\000\037\000\029\000\029\000\038\000\035\000\
\052\000\005\000\023\000\035\000\035\000\064\000\095\000\091\000\
\035\000\029\000\074\000\035\000\046\000\029\000\029\000\030\000\
\030\000\023\000\029\000\047\000\039\000\029\000\018\000\105\000\
\025\000\031\000\031\000\018\000\030\000\025\000\021\000\020\000\
\030\000\030\000\022\000\021\000\020\000\030\000\031\000\022\000\
\030\000\080\000\031\000\031\000\007\000\017\000\083\000\031\000\
\018\000\027\000\031\000\050\000\051\000\008\000\019\000\009\000\
\010\000\020\000\011\000\028\000\030\000\027\000\012\000\029\000\
\032\000\027\000\027\000\037\000\013\000\035\000\040\000\028\000\
\041\000\027\000\045\000\028\000\028\000\048\000\024\000\028\000\
\053\000\055\000\056\000\028\000\031\000\057\000\033\000\034\000\
\058\000\067\000\075\000\072\000\073\000\076\000\077\000\078\000\
\081\000\079\000\085\000\087\000\089\000\084\000\088\000\099\000\
\094\000\101\000\096\000\100\000\098\000\097\000\090\000\002\000\
\104\000\103\000\106\000\016\000\054\000\006\000\040\000\102\000\
\044\000\049\000\082\000\000\000\012\000"

let yycheck = "\056\000\
\057\000\001\001\002\001\001\001\001\000\008\001\007\001\023\000\
\016\001\003\001\004\001\005\001\005\001\016\001\010\001\011\001\
\001\001\002\001\005\001\005\001\004\001\005\001\004\001\017\001\
\040\000\006\001\026\001\021\001\022\001\029\001\087\000\029\001\
\026\001\017\001\021\001\029\001\029\001\021\001\022\001\004\001\
\005\001\026\001\026\001\029\001\026\001\029\001\017\001\104\000\
\017\001\004\001\005\001\022\001\017\001\022\001\017\001\017\001\
\021\001\022\001\017\001\022\001\022\001\026\001\017\001\022\001\
\029\001\071\000\021\001\022\001\001\001\018\001\076\000\026\001\
\001\001\005\001\029\001\038\000\039\000\010\001\028\001\012\001\
\013\001\028\001\015\001\005\001\022\001\017\001\019\001\017\001\
\023\001\021\001\022\001\005\001\025\001\020\001\003\001\017\001\
\007\001\029\001\001\001\021\001\022\001\001\001\011\000\012\000\
\001\001\022\001\016\001\029\001\017\000\016\001\019\000\020\000\
\028\001\001\001\029\001\017\001\017\001\016\001\001\001\001\001\
\014\001\017\001\028\001\016\001\001\001\023\001\017\001\001\001\
\023\001\001\001\022\001\017\001\021\001\023\001\001\001\000\000\
\016\001\018\001\017\001\017\001\043\000\017\001\029\001\098\000\
\030\000\037\000\074\000\255\255\023\001"

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
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 27 "parser.mly"
                                              ( Program (MainDecl(_4), _6) )
# 237 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                        ( [] )
# 243 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                   ( _1 :: _2 )
# 251 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 36 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 259 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 37 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 268 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                        ( [] )
# 274 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 41 "parser.mly"
                                      ( _1 :: _2 )
# 282 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 44 "parser.mly"
                                                          ( MethDecl ((methodDesc _2 _5), _3, _8) )
# 292 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                              ( ClassVarDecl (variableDesc _2 _4) )
# 300 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                   ( [] )
# 306 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "parser.mly"
                           ( _2 )
# 313 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 52 "parser.mly"
                ( [_1] )
# 320 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "parser.mly"
                                 ( _1 :: _3 )
# 328 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                         ( Formal (_1, _3) )
# 336 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                   ( seq _1 )
# 343 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                  ( [_1] )
# 350 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
                         ( _1 :: _3 )
# 358 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                   ( Skip )
# 364 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 69 "parser.mly"
                              ( LocalVarDecl (variableDesc _2 _4) )
# 372 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                        ( AssignStmt (_1, _3) )
# 380 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                    ( ReturnStmt _2 )
# 387 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 72 "parser.mly"
                                         ( IfStmt (_3, _6, Skip) )
# 395 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 73 "parser.mly"
                                                                ( IfStmt(_3, _6, _10) )
# 404 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 74 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 412 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                   ( PrintStmt (_2) )
# 419 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                 ( Newline )
# 425 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 81 "parser.mly"
                ( exprDesc _1 )
# 432 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 82 "parser.mly"
                         ( exprDesc (Binop (_2, _1, _3)) )
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 85 "parser.mly"
                ( exprDesc _1 )
# 448 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 86 "parser.mly"
                         ( Binop (_2, _1, _3) )
# 457 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 87 "parser.mly"
                         ( Binop (Minus, _1, _3) )
# 465 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 90 "parser.mly"
                 ( exprDesc _1 )
# 472 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 91 "parser.mly"
                         ( Binop (_2, _1, _3) )
# 481 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "parser.mly"
                ( Number _1 )
# 488 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "parser.mly"
                ( Variable _1 )
# 495 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 96 "parser.mly"
                            ( Call (_1, _3, _4) )
# 504 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 97 "parser.mly"
                     ( Monop (Uminus, _2))
# 511 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
                     ( [] )
# 517 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 101 "parser.mly"
                                ( _2 )
# 524 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
               ( [_1] )
# 531 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 105 "parser.mly"
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
