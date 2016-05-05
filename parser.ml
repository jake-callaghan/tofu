type token =
  | IDENT of (string)
  | NUMBER of (int)
  | TRUE
  | FALSE
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
# 35 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* MAIN *);
  262 (* CLASS *);
  263 (* EXTENDS *);
  264 (* NEW *);
  265 (* VAR *);
  266 (* DEF *);
  267 (* WHILE *);
  268 (* IF *);
  269 (* ELSE *);
  270 (* PRINT *);
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
\012\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\005\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\004\000\001\000\004\000\001\000\001\000\001\000\001\000\002\000\
\004\000\002\000\003\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\028\000\029\000\
\030\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
\000\000\015\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\017\000\000\000\019\000\000\000\000\000\025\000\000\000\000\000\
\003\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\035\000\024\000\000\000\000\000\000\000\
\000\000\004\000\007\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\000\000\000\000\000\010\000\000\000\000\000\
\000\000\000\000\009\000\000\000\011\000\000\000\000\000\023\000\
\014\000\013\000\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\017\000\039\000\040\000\065\000\066\000\081\000\087\000\
\088\000\018\000\019\000\020\000\051\000\059\000"

let yysindex = "\006\000\
\025\255\000\000\021\255\000\000\039\255\041\255\000\000\000\000\
\000\000\037\255\054\255\043\255\045\255\046\255\019\255\000\000\
\048\255\000\000\051\255\047\255\019\255\000\000\052\255\019\255\
\019\255\019\255\000\000\050\255\067\255\039\255\074\255\050\255\
\075\255\254\254\255\254\007\255\076\255\077\255\000\000\067\255\
\000\000\055\255\000\000\064\255\066\255\000\000\055\255\001\255\
\000\000\011\255\000\000\039\255\039\255\000\000\081\255\251\254\
\000\000\026\255\057\255\068\255\069\255\071\255\086\255\087\255\
\073\255\251\254\019\255\000\000\000\000\078\255\251\254\070\255\
\065\255\000\000\000\000\000\000\079\255\080\255\092\255\002\255\
\082\255\039\255\000\000\084\255\085\255\000\000\072\255\083\255\
\094\255\090\255\000\000\096\255\000\000\098\255\091\255\000\000\
\000\000\000\000\095\255\039\255\093\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\023\255\047\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\097\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\255\100\000\023\255\000\000\040\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\100\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\255\023\255\023\255\000\000\000\000\099\255\
\000\000\088\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\099\255\000\000\000\000\000\000\044\255\099\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\023\255\000\000\000\000\000\000\000\000\000\000\100\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\205\255\061\000\000\000\252\255\000\000\000\000\008\000\
\000\000\081\000\000\000\241\255\065\000\049\000"

let yytablesize = 126
let yytable = "\028\000\
\060\000\061\000\085\000\063\000\064\000\032\000\001\000\055\000\
\034\000\035\000\036\000\027\000\007\000\008\000\009\000\056\000\
\037\000\037\000\010\000\027\000\007\000\008\000\009\000\044\000\
\045\000\037\000\010\000\086\000\027\000\003\000\090\000\033\000\
\046\000\027\000\058\000\005\000\057\000\022\000\018\000\006\000\
\007\000\008\000\009\000\018\000\037\000\067\000\010\000\011\000\
\101\000\012\000\013\000\058\000\014\000\021\000\023\000\020\000\
\015\000\021\000\021\000\022\000\020\000\075\000\016\000\029\000\
\022\000\031\000\078\000\024\000\037\000\025\000\026\000\030\000\
\038\000\033\000\042\000\043\000\047\000\048\000\052\000\050\000\
\053\000\062\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\080\000\077\000\079\000\084\000\082\000\095\000\083\000\
\097\000\093\000\085\000\002\000\049\000\098\000\094\000\089\000\
\091\000\096\000\092\000\099\000\102\000\100\000\041\000\054\000\
\016\000\036\000\006\000\076\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000"

let yycheck = "\015\000\
\052\000\053\000\001\001\009\001\010\001\021\000\001\000\007\001\
\024\000\025\000\026\000\001\001\002\001\003\001\004\001\015\001\
\019\001\019\001\008\001\001\001\002\001\003\001\004\001\026\001\
\026\001\019\001\008\001\026\001\016\001\005\001\082\000\019\001\
\026\001\021\001\050\000\015\001\026\001\001\001\016\001\001\001\
\002\001\003\001\004\001\021\001\019\001\020\001\008\001\009\001\
\100\000\011\001\012\001\067\000\014\001\016\001\001\001\016\001\
\018\001\017\001\021\001\016\001\021\001\066\000\024\001\016\001\
\021\001\019\001\071\000\025\001\019\001\025\001\025\001\021\001\
\006\001\022\001\001\001\001\001\001\001\001\001\015\001\025\001\
\015\001\001\001\026\001\016\001\016\001\015\001\001\001\001\001\
\016\001\025\001\013\001\022\001\001\001\015\001\001\001\016\001\
\001\001\026\001\001\001\000\000\040\000\094\000\020\001\022\001\
\021\001\016\001\022\001\017\001\016\001\015\001\030\000\047\000\
\016\001\026\001\016\001\067\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\026\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
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
# 214 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                        ( [] )
# 220 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 30 "parser.mly"
                                   ( _1 :: _2 )
# 228 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 236 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 34 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 245 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                        ( [] )
# 251 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 38 "parser.mly"
                                      ( _1 :: _2 )
# 259 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "parser.mly"
                                                          ( MethDecl (methodDesc _2 _5 _3 _8) )
# 269 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                              ( InstanceVarDecl ((variableDesc _2),_4) )
# 277 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "parser.mly"
                   ( [] )
# 283 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 46 "parser.mly"
                               ( _2 )
# 290 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 49 "parser.mly"
                ( [_1] )
# 297 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 50 "parser.mly"
                                 ( _1 :: _3 )
# 305 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                         ( Formal (_1, _3) )
# 313 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 58 "parser.mly"
                   ( seq _1 )
# 320 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 61 "parser.mly"
                  ( [_1] )
# 327 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 62 "parser.mly"
                         ( _1 :: _3 )
# 335 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "parser.mly"
                   ( Skip )
# 341 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 66 "parser.mly"
                              ( LocalVarDecl ((variableDesc _2),_4) )
# 349 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                            ( AssignStmt (_1, _3) )
# 357 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                         ( ReturnStmt _2 )
# 364 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                         ( IfStmt (_3, _6, Skip) )
# 372 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                                                ( IfStmt(_3, _6, _10) )
# 381 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 71 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 389 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                              ( PrintStmt (_3) )
# 396 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                 ( Newline )
# 402 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 74 "parser.mly"
                          ( UnitCall (_1, _3, _4) )
# 411 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
                  ( exprDesc (Number _1)  )
# 418 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                 ( exprDesc (Boolean true) )
# 424 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                  ( exprDesc (Boolean false) )
# 430 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 82 "parser.mly"
                   ( exprDesc (Variable (variableDesc _1)) )
# 437 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 83 "parser.mly"
                   ( exprDesc (NewObject _2) )
# 444 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 84 "parser.mly"
                         ( exprDesc (Call (_1, _3, _4)) )
# 453 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                     ( [] )
# 459 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 88 "parser.mly"
                                ( _2 )
# 466 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
               ( [_1] )
# 473 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 92 "parser.mly"
                           ( _1 :: _3 )
# 481 "parser.ml"
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
