type token =
  | IDENT of (string)
  | NUMBER of (int)
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
# 17 "parser.mly"
	open Tree
# 33 "parser.ml"
let yytransl_const = [|
  259 (* MAIN *);
  260 (* CLASS *);
  261 (* EXTENDS *);
  262 (* NEW *);
  263 (* VAR *);
  264 (* DEF *);
  265 (* WHILE *);
  266 (* IF *);
  267 (* ELSE *);
  268 (* PRINT *);
  269 (* LCURL *);
  270 (* RCURL *);
  271 (* ASSIGN *);
  272 (* RETURN *);
  273 (* DOT *);
  274 (* COMMA *);
  275 (* SEMI *);
  276 (* COLON *);
    0 (* EOF *);
  277 (* BADTOK *);
  278 (* NEWLINE *);
  279 (* LBRAC *);
  280 (* RBRAC *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* NUMBER *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\005\000\005\000\006\000\
\006\000\007\000\007\000\008\000\008\000\009\000\002\000\010\000\
\010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\013\000\
\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\005\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\004\000\001\000\004\000\001\000\001\000\002\000\004\000\002\000\
\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\036\000\000\000\000\000\028\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\015\000\
\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\017\000\000\000\
\019\000\000\000\000\000\025\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\031\000\000\000\000\000\032\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\024\000\000\000\000\000\000\000\000\000\004\000\
\007\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\009\000\000\000\011\000\000\000\000\000\023\000\014\000\013\000\
\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\015\000\037\000\038\000\063\000\064\000\079\000\085\000\
\086\000\016\000\017\000\018\000\049\000\057\000"

let yysindex = "\006\000\
\031\255\000\000\038\255\000\000\011\255\039\255\000\000\057\255\
\058\255\040\255\041\255\042\255\037\255\000\000\046\255\000\000\
\043\255\044\255\037\255\000\000\047\255\037\255\037\255\037\255\
\000\000\051\255\062\255\011\255\068\255\051\255\069\255\008\255\
\012\255\013\255\070\255\071\255\000\000\062\255\000\000\050\255\
\000\000\061\255\063\255\000\000\050\255\009\255\000\000\002\255\
\000\000\011\255\011\255\000\000\074\255\049\255\000\000\254\254\
\053\255\064\255\065\255\067\255\080\255\081\255\072\255\049\255\
\037\255\000\000\000\000\073\255\049\255\075\255\060\255\000\000\
\000\000\000\000\076\255\077\255\084\255\004\255\078\255\011\255\
\000\000\082\255\079\255\000\000\066\255\085\255\086\255\083\255\
\000\000\087\255\000\000\091\255\089\255\000\000\000\000\000\000\
\092\255\011\255\088\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\005\255\044\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\093\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\255\093\000\005\255\000\000\028\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\093\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\255\005\255\005\255\000\000\000\000\094\255\000\000\090\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\094\255\
\000\000\000\000\000\000\034\255\094\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\000\000\000\000\000\000\000\000\000\000\095\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\207\255\056\000\000\000\242\255\000\000\000\000\004\000\
\000\000\072\000\000\000\243\255\061\000\044\000"

let yytablesize = 119
let yytable = "\026\000\
\058\000\059\000\025\000\007\000\083\000\030\000\001\000\008\000\
\032\000\033\000\034\000\006\000\007\000\053\000\035\000\065\000\
\008\000\009\000\018\000\010\000\011\000\054\000\012\000\018\000\
\035\000\055\000\013\000\084\000\035\000\035\000\088\000\042\000\
\014\000\003\000\056\000\043\000\044\000\025\000\007\000\021\000\
\027\000\020\000\008\000\031\000\021\000\027\000\020\000\022\000\
\099\000\073\000\005\000\056\000\022\000\019\000\076\000\061\000\
\062\000\020\000\021\000\027\000\029\000\028\000\022\000\023\000\
\024\000\036\000\031\000\035\000\040\000\041\000\045\000\046\000\
\048\000\050\000\060\000\051\000\066\000\067\000\068\000\069\000\
\070\000\071\000\078\000\075\000\082\000\072\000\093\000\095\000\
\080\000\091\000\081\000\083\000\002\000\047\000\077\000\096\000\
\094\000\087\000\090\000\039\000\089\000\100\000\092\000\097\000\
\098\000\052\000\016\000\006\000\074\000\000\000\000\000\000\000\
\000\000\034\000\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\013\000\
\050\000\051\000\001\001\002\001\001\001\019\000\001\000\006\001\
\022\000\023\000\024\000\001\001\002\001\005\001\017\001\018\001\
\006\001\007\001\014\001\009\001\010\001\013\001\012\001\019\001\
\017\001\024\001\016\001\024\001\017\001\017\001\080\000\024\001\
\022\001\003\001\048\000\024\001\024\001\001\001\002\001\014\001\
\014\001\014\001\006\001\017\001\019\001\019\001\019\001\014\001\
\098\000\064\000\013\001\065\000\019\001\015\001\069\000\007\001\
\008\001\001\001\001\001\014\001\017\001\019\001\023\001\023\001\
\023\001\004\001\020\001\017\001\001\001\001\001\001\001\001\001\
\023\001\013\001\001\001\013\001\024\001\014\001\014\001\013\001\
\001\001\001\001\023\001\011\001\001\001\014\001\001\001\001\001\
\013\001\024\001\014\001\001\001\000\000\038\000\020\001\092\000\
\014\001\020\001\020\001\028\000\019\001\014\001\018\001\015\001\
\013\001\045\000\014\001\014\001\065\000\255\255\255\255\255\255\
\255\255\024\001\255\255\255\255\255\255\255\255\024\001"

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
# 23 "parser.mly"
                                        ( Program (MainBody(_3), _5) )
# 206 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                        ( [] )
# 212 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 29 "parser.mly"
                                   ( _1 :: _2 )
# 220 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 32 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 228 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 237 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                        ( [] )
# 243 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 37 "parser.mly"
                                      ( _1 :: _2 )
# 251 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 40 "parser.mly"
                                                          ( MethDecl (methodDesc _2 _5 _3 _8) )
# 261 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 41 "parser.mly"
                              ( InstanceVarDecl ((variableDesc _2),_4) )
# 269 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                   ( [] )
# 275 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 45 "parser.mly"
                               ( _2 )
# 282 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 48 "parser.mly"
                ( [_1] )
# 289 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "parser.mly"
                                 ( _1 :: _3 )
# 297 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                         ( Formal (_1, _3) )
# 305 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
                   ( seq _1 )
# 312 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                  ( [_1] )
# 319 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                         ( _1 :: _3 )
# 327 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                   ( Skip )
# 333 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                              ( LocalVarDecl ((variableDesc _2),_4) )
# 341 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                            ( AssignStmt (_1, _3) )
# 349 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                         ( ReturnStmt _2 )
# 356 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
                                         ( IfStmt (_3, _6, Skip) )
# 364 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                                                ( IfStmt(_3, _6, _10) )
# 373 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 381 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                              ( PrintStmt (_3) )
# 388 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                 ( Newline )
# 394 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 73 "parser.mly"
                          ( UnitCall (_1, _3, _4) )
# 403 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
                 ( exprDesc (Number _1) )
# 410 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
                ( exprDesc (Variable (variableDesc _1)) )
# 417 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
                   ( exprDesc (NewObject _2) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 81 "parser.mly"
                           ( exprDesc (Call (_1, _3, _4)) )
# 433 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                     ( [] )
# 439 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 85 "parser.mly"
                                ( _2 )
# 446 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 88 "parser.mly"
               ( [_1] )
# 453 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 89 "parser.mly"
                           ( _1 :: _3 )
# 461 "parser.ml"
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
