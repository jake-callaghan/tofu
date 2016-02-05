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
\011\000\011\000\012\000\012\000\012\000\013\000\013\000\013\000\
\014\000\014\000\015\000\015\000\015\000\015\000\016\000\016\000\
\017\000\017\000\000\000"

let yylen = "\002\000\
\006\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\002\000\001\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\001\000\001\000\004\000\002\000\002\000\003\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\017\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\003\000\
\000\000\000\000\000\000\037\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\000\000\040\000\000\000\000\000\000\000\004\000\
\007\000\000\000\042\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\009\000\000\000\011\000\000\000\000\000\023\000\014\000\013\000\
\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\014\000\043\000\044\000\071\000\072\000\087\000\093\000\
\094\000\015\000\016\000\066\000\025\000\026\000\027\000\060\000\
\067\000"

let yysindex = "\005\000\
\056\255\000\000\002\255\000\000\255\254\065\255\028\255\091\255\
\072\255\075\255\015\255\015\255\000\000\093\255\000\000\086\255\
\015\255\094\255\015\255\015\255\096\255\000\000\015\255\052\255\
\016\255\116\255\000\000\052\255\117\255\065\255\052\255\124\255\
\038\255\043\255\125\255\000\000\015\255\015\255\015\255\015\255\
\015\255\126\255\000\000\117\255\000\000\000\000\112\255\113\255\
\102\255\016\255\016\255\116\255\116\255\000\000\253\254\000\000\
\065\255\065\255\001\255\000\000\130\255\008\255\115\255\118\255\
\000\000\084\255\104\255\120\255\133\255\136\255\121\255\008\255\
\000\000\127\255\015\255\000\000\008\255\119\255\111\255\000\000\
\000\000\128\255\000\000\123\255\142\255\003\255\122\255\065\255\
\000\000\129\255\131\255\000\000\132\255\134\255\145\255\135\255\
\000\000\146\255\000\000\147\255\138\255\000\000\000\000\000\000\
\137\255\065\255\140\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\248\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\141\255\
\000\000\000\000\000\000\000\000\007\255\000\000\000\000\004\255\
\066\255\018\255\000\000\036\255\149\000\248\254\039\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\149\000\000\000\000\000\000\000\000\000\
\000\000\077\255\080\255\033\255\047\255\000\000\000\000\000\000\
\248\254\248\254\000\000\000\000\000\000\143\255\000\000\000\000\
\000\000\139\255\000\000\000\000\000\000\000\000\000\000\143\255\
\000\000\064\255\000\000\000\000\143\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\248\254\
\000\000\000\000\000\000\000\000\000\000\144\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\248\254\000\000\000\000"

let yygindex = "\000\000\
\000\000\199\255\106\000\000\000\019\000\000\000\000\000\059\000\
\000\000\132\000\000\000\101\000\077\000\083\000\240\255\000\000\
\088\000"

let yytablesize = 173
let yytable = "\063\000\
\064\000\021\000\022\000\091\000\061\000\001\000\036\000\005\000\
\018\000\036\000\036\000\036\000\062\000\018\000\006\000\021\000\
\022\000\069\000\070\000\039\000\025\000\030\000\030\000\036\000\
\054\000\025\000\023\000\036\000\036\000\065\000\096\000\092\000\
\036\000\036\000\030\000\036\000\031\000\031\000\030\000\030\000\
\023\000\040\000\037\000\030\000\030\000\017\000\030\000\037\000\
\107\000\031\000\032\000\032\000\021\000\031\000\031\000\020\000\
\037\000\021\000\031\000\031\000\020\000\031\000\003\000\032\000\
\038\000\007\000\047\000\032\000\032\000\038\000\027\000\048\000\
\032\000\032\000\008\000\032\000\009\000\010\000\038\000\011\000\
\022\000\028\000\027\000\012\000\029\000\022\000\027\000\027\000\
\037\000\013\000\081\000\018\000\027\000\028\000\027\000\084\000\
\029\000\028\000\028\000\019\000\029\000\029\000\020\000\028\000\
\075\000\028\000\029\000\030\000\029\000\029\000\038\000\024\000\
\028\000\050\000\051\000\035\000\032\000\031\000\041\000\033\000\
\034\000\052\000\053\000\042\000\046\000\049\000\055\000\057\000\
\058\000\059\000\068\000\073\000\076\000\078\000\074\000\077\000\
\079\000\080\000\086\000\089\000\082\000\085\000\090\000\088\000\
\095\000\101\000\103\000\091\000\002\000\056\000\097\000\102\000\
\106\000\098\000\100\000\105\000\108\000\016\000\104\000\006\000\
\099\000\045\000\083\000\000\000\000\000\000\000\000\000\041\000\
\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\057\000\
\058\000\001\001\002\001\001\001\008\001\001\000\023\000\006\001\
\017\001\003\001\004\001\005\001\016\001\022\001\016\001\001\001\
\002\001\010\001\011\001\004\001\017\001\004\001\005\001\017\001\
\041\000\022\001\026\001\021\001\022\001\029\001\088\000\029\001\
\026\001\027\001\017\001\029\001\004\001\005\001\021\001\022\001\
\026\001\026\001\005\001\026\001\027\001\018\001\029\001\005\001\
\106\000\017\001\004\001\005\001\017\001\021\001\022\001\017\001\
\005\001\022\001\026\001\027\001\022\001\029\001\007\001\017\001\
\027\001\001\001\029\001\021\001\022\001\027\001\005\001\029\001\
\026\001\027\001\010\001\029\001\012\001\013\001\027\001\015\001\
\017\001\005\001\017\001\019\001\005\001\022\001\021\001\022\001\
\005\001\025\001\072\000\001\001\027\001\017\001\029\001\077\000\
\017\001\021\001\022\001\028\001\021\001\022\001\028\001\027\001\
\021\001\029\001\027\001\022\001\029\001\017\001\027\001\011\000\
\012\000\037\000\038\000\020\001\023\001\017\000\003\001\019\000\
\020\000\039\000\040\000\007\001\001\001\001\001\001\001\016\001\
\016\001\028\001\001\001\017\001\029\001\001\001\017\001\016\001\
\001\001\017\001\028\001\017\001\014\001\023\001\001\001\016\001\
\023\001\001\001\001\001\001\001\000\000\044\000\022\001\017\001\
\016\001\023\001\021\001\018\001\017\001\017\001\100\000\017\001\
\029\001\030\000\075\000\255\255\255\255\255\255\255\255\029\001\
\255\255\255\255\255\255\255\255\029\001"

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
# 243 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                        ( [] )
# 249 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                   ( _1 :: _2 )
# 257 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 36 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 265 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 37 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 274 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                        ( [] )
# 280 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 41 "parser.mly"
                                      ( _1 :: _2 )
# 288 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 44 "parser.mly"
                                                          ( MethDecl ((methodDesc _2 _5), _3, _8) )
# 298 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                              ( ClassVarDecl (variableDesc _2 _4) )
# 306 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                   ( [] )
# 312 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 49 "parser.mly"
                               ( _2 )
# 319 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 52 "parser.mly"
                ( [_1] )
# 326 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 53 "parser.mly"
                                 ( _1 :: _3 )
# 334 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "parser.mly"
                         ( Formal (_1, _3) )
# 342 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                   ( seq _1 )
# 349 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                  ( [_1] )
# 356 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 65 "parser.mly"
                         ( _1 :: _3 )
# 364 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                   ( Skip )
# 370 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 69 "parser.mly"
                              ( LocalVarDecl (variableDesc _2 _4) )
# 378 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                            ( AssignStmt (_1, _3) )
# 386 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                         ( ReturnStmt _2 )
# 393 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 72 "parser.mly"
                                         ( IfStmt (_3, _6, Skip) )
# 401 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 73 "parser.mly"
                                                                ( IfStmt(_3, _6, _10) )
# 410 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 74 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 418 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                      ( PrintStmt (_2) )
# 425 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                 ( Newline )
# 431 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 81 "parser.mly"
                ( _1 )
# 438 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 82 "parser.mly"
                         ( exprDesc (Binop (_2, _1, _3)) )
# 447 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'simple) in
    Obj.repr(
# 83 "parser.mly"
                         ( exprDesc (Binop (Eq, _1, _3)) )
# 455 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 86 "parser.mly"
                ( _1 )
# 462 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 87 "parser.mly"
                         ( exprDesc (Binop (_2, _1, _3)) )
# 471 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'simple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 88 "parser.mly"
                         ( exprDesc (Binop (Minus, _1, _3)) )
# 479 "parser.ml"
               : 'simple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 91 "parser.mly"
                 ( _1 )
# 486 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 92 "parser.mly"
                         ( exprDesc (Binop (_2, _1, _3)) )
# 495 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
                ( exprDesc (Number _1) )
# 502 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                ( exprDesc (Variable _1) )
# 509 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 97 "parser.mly"
                            ( exprDesc (Call (_1, _3, _4)) )
# 518 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 98 "parser.mly"
                     ( exprDesc (Monop (Uminus, _2)) )
# 525 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                     ( [] )
# 531 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 102 "parser.mly"
                                ( _2 )
# 538 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
               ( [_1] )
# 545 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 106 "parser.mly"
                           ( _1 :: _3 )
# 553 "parser.ml"
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
