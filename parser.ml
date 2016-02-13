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
\011\000\011\000\012\000\012\000\012\000\013\000\013\000\014\000\
\014\000\000\000"

let yylen = "\002\000\
\006\000\000\000\002\000\005\000\007\000\000\000\002\000\009\000\
\005\000\002\000\003\000\001\000\003\000\003\000\001\000\001\000\
\003\000\000\000\004\000\003\000\002\000\007\000\011\000\007\000\
\002\000\001\000\001\000\001\000\004\000\002\000\003\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\026\000\000\000\015\000\000\000\
\000\000\000\000\000\000\000\000\028\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\017\000\019\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
\000\000\030\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\000\000\031\000\000\000\000\000\000\000\
\004\000\007\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\009\000\000\000\011\000\000\000\000\000\023\000\014\000\
\013\000\000\000\000\000\000\000\008\000"

let yydgoto = "\002\000\
\004\000\014\000\033\000\034\000\056\000\057\000\072\000\078\000\
\079\000\015\000\016\000\051\000\045\000\052\000"

let yysindex = "\007\000\
\038\255\000\000\019\255\000\000\011\255\004\255\033\255\049\255\
\028\255\030\255\005\255\005\255\000\000\040\255\000\000\036\255\
\005\255\032\255\005\255\005\255\000\000\000\000\039\255\039\255\
\053\255\004\255\039\255\057\255\251\254\006\255\058\255\059\255\
\000\000\053\255\000\000\000\000\048\255\050\255\041\255\016\255\
\000\000\004\255\004\255\001\255\000\000\061\255\037\255\051\255\
\052\255\000\000\029\255\043\255\055\255\068\255\069\255\060\255\
\037\255\000\000\062\255\005\255\000\000\037\255\056\255\054\255\
\000\000\000\000\065\255\000\000\066\255\070\255\003\255\063\255\
\004\255\000\000\067\255\064\255\000\000\071\255\072\255\074\255\
\073\255\000\000\078\255\000\000\080\255\076\255\000\000\000\000\
\000\000\075\255\004\255\079\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\014\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\082\255\
\000\000\000\000\000\000\000\000\000\000\000\000\018\255\020\255\
\072\000\014\255\021\255\000\000\000\000\000\000\000\000\000\000\
\000\000\072\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\014\255\014\255\000\000\000\000\000\000\083\255\000\000\
\000\000\000\000\077\255\000\000\000\000\000\000\000\000\000\000\
\083\255\000\000\022\255\000\000\000\000\083\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\255\000\000\000\000\000\000\000\000\000\000\081\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\214\255\048\000\000\000\237\255\000\000\000\000\004\000\
\000\000\059\000\000\000\254\255\000\000\032\000"

let yytablesize = 105
let yytable = "\048\000\
\049\000\021\000\022\000\076\000\007\000\021\000\022\000\001\000\
\023\000\024\000\008\000\031\000\009\000\010\000\027\000\011\000\
\029\000\030\000\037\000\012\000\046\000\005\000\031\000\006\000\
\050\000\013\000\077\000\018\000\047\000\038\000\081\000\025\000\
\018\000\021\000\020\000\022\000\025\000\066\000\021\000\020\000\
\022\000\003\000\069\000\054\000\055\000\031\000\060\000\017\000\
\092\000\018\000\019\000\028\000\020\000\025\000\026\000\031\000\
\032\000\036\000\039\000\040\000\042\000\053\000\043\000\044\000\
\058\000\059\000\061\000\062\000\063\000\064\000\075\000\002\000\
\067\000\065\000\086\000\070\000\071\000\073\000\088\000\074\000\
\076\000\041\000\080\000\083\000\035\000\082\000\087\000\091\000\
\089\000\085\000\090\000\068\000\093\000\000\000\084\000\016\000\
\006\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\012\000"

let yycheck = "\042\000\
\043\000\001\001\002\001\001\001\001\001\001\001\002\001\001\000\
\011\000\012\000\007\001\017\001\009\001\010\001\017\000\012\001\
\019\000\020\000\024\001\016\001\005\001\003\001\017\001\013\001\
\024\001\022\001\024\001\014\001\013\001\024\001\073\000\014\001\
\019\001\014\001\014\001\014\001\019\001\057\000\019\001\019\001\
\019\001\004\001\062\000\007\001\008\001\017\001\018\001\015\001\
\091\000\001\001\023\001\020\001\023\001\014\001\019\001\017\001\
\004\001\001\001\001\001\001\001\013\001\001\001\013\001\023\001\
\014\001\014\001\024\001\013\001\001\001\001\001\001\001\000\000\
\011\001\014\001\001\001\020\001\023\001\013\001\001\001\014\001\
\001\001\034\000\020\001\020\001\026\000\019\001\014\001\013\001\
\085\000\018\001\015\001\060\000\014\001\255\255\024\001\014\001\
\014\001\255\255\255\255\255\255\024\001\255\255\255\255\255\255\
\024\001"

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
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 23 "parser.mly"
                                              ( Program (MainDecl(_4), _6) )
# 201 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                        ( [] )
# 207 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'class_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'class_decl_list) in
    Obj.repr(
# 29 "parser.mly"
                                   ( _1 :: _2 )
# 215 "parser.ml"
               : 'class_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 32 "parser.mly"
                                                   ( ClassDecl (classDesc _2 "Object", _4) )
# 223 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl_list) in
    Obj.repr(
# 33 "parser.mly"
                                                            ( ClassDecl (classDesc _2 _4, _6) )
# 232 "parser.ml"
               : 'class_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 36 "parser.mly"
                        ( [] )
# 238 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'feature_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'feature_decl_list) in
    Obj.repr(
# 37 "parser.mly"
                                      ( _1 :: _2 )
# 246 "parser.ml"
               : 'feature_decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'formals) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 40 "parser.mly"
                                                          ( MethDecl ((methodDesc _2 _5), _3, _8) )
# 256 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 41 "parser.mly"
                              ( ClassVarDecl (variableDesc _2 _4) )
# 264 "parser.ml"
               : 'feature_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                   ( [] )
# 270 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_list) in
    Obj.repr(
# 45 "parser.mly"
                               ( _2 )
# 277 "parser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 48 "parser.mly"
                ( [_1] )
# 284 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "parser.mly"
                                 ( _1 :: _3 )
# 292 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                         ( Formal (_1, _3) )
# 300 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 57 "parser.mly"
                   ( seq _1 )
# 307 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 60 "parser.mly"
                  ( [_1] )
# 314 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                         ( _1 :: _3 )
# 322 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                   ( Skip )
# 328 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 65 "parser.mly"
                              ( LocalVarDecl (variableDesc _2 _4) )
# 336 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                            ( AssignStmt (_1, _3) )
# 344 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                         ( ReturnStmt _2 )
# 351 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 68 "parser.mly"
                                         ( IfStmt (_3, _6, Skip) )
# 359 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'stmts) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 69 "parser.mly"
                                                                ( IfStmt(_3, _6, _10) )
# 368 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 70 "parser.mly"
                                            ( WhileStmt (_3, _6) )
# 376 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                      ( PrintStmt (_2) )
# 383 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                 ( Newline )
# 389 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "parser.mly"
                 ( exprDesc (Number _1) )
# 396 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 78 "parser.mly"
                ( exprDesc (Variable _1) )
# 403 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 79 "parser.mly"
                           ( exprDesc (Call (_1, _3, _4)) )
# 412 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                     ( [] )
# 418 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 83 "parser.mly"
                                ( _2 )
# 425 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 86 "parser.mly"
               ( [_1] )
# 432 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 87 "parser.mly"
                           ( _1 :: _3 )
# 440 "parser.ml"
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
