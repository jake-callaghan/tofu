type token =
  | IDENT of (Dict.ident)
  | MULOP of (Keiko.op)
  | ADDOP of (Keiko.op)
  | RELOP of (Keiko.op)
  | NUMBER of (int)
  | CHAR of (char)
  | STRING of (Keiko.symbol * int)
  | SEMI
  | DOT
  | COLON
  | LPAR
  | RPAR
  | COMMA
  | SUB
  | BUS
  | EQUAL
  | MINUS
  | ASSIGN
  | VBAR
  | ARROW
  | BADTOK
  | IMPOSSIBLE
  | ARRAY
  | BEGIN
  | CONST
  | DO
  | ELSE
  | END
  | IF
  | OF
  | PROC
  | RECORD
  | RETURN
  | THEN
  | TO
  | TYPE
  | VAR
  | WHILE
  | NOT
  | POINTER
  | NIL
  | REPEAT
  | UNTIL
  | FOR
  | ELSIF
  | CASE

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Keiko
open Dict
open Tree
# 56 "parser.ml"
let yytransl_const = [|
  264 (* SEMI *);
  265 (* DOT *);
  266 (* COLON *);
  267 (* LPAR *);
  268 (* RPAR *);
  269 (* COMMA *);
  270 (* SUB *);
  271 (* BUS *);
  272 (* EQUAL *);
  273 (* MINUS *);
  274 (* ASSIGN *);
  275 (* VBAR *);
  276 (* ARROW *);
  277 (* BADTOK *);
  278 (* IMPOSSIBLE *);
  279 (* ARRAY *);
  280 (* BEGIN *);
  281 (* CONST *);
  282 (* DO *);
  283 (* ELSE *);
  284 (* END *);
  285 (* IF *);
  286 (* OF *);
  287 (* PROC *);
  288 (* RECORD *);
  289 (* RETURN *);
  290 (* THEN *);
  291 (* TO *);
  292 (* TYPE *);
  293 (* VAR *);
  294 (* WHILE *);
  295 (* NOT *);
  296 (* POINTER *);
  297 (* NIL *);
  298 (* REPEAT *);
  299 (* UNTIL *);
  300 (* FOR *);
  301 (* ELSIF *);
  302 (* CASE *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* MULOP *);
  259 (* ADDOP *);
  260 (* RELOP *);
  261 (* NUMBER *);
  262 (* CHAR *);
  263 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\005\000\005\000\005\000\
\006\000\006\000\010\000\009\000\009\000\012\000\007\000\007\000\
\014\000\008\000\016\000\018\000\018\000\020\000\020\000\021\000\
\021\000\021\000\019\000\019\000\004\000\022\000\022\000\023\000\
\023\000\024\000\025\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\029\000\029\000\029\000\030\000\030\000\
\032\000\031\000\031\000\015\000\015\000\028\000\028\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\027\000\027\000\033\000\
\033\000\026\000\026\000\026\000\026\000\013\000\013\000\013\000\
\013\000\034\000\034\000\035\000\036\000\036\000\017\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\002\000\002\000\002\000\001\000\002\000\
\001\000\002\000\004\000\001\000\002\000\004\000\001\000\002\000\
\004\000\004\000\004\000\002\000\003\000\001\000\003\000\003\000\
\004\000\001\000\000\000\002\000\001\000\001\000\003\000\002\000\
\001\000\000\000\000\000\003\000\002\000\002\000\006\000\005\000\
\004\000\009\000\006\000\000\000\002\000\006\000\001\000\003\000\
\003\000\000\000\002\000\001\000\003\000\000\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\003\000\001\000\
\003\000\001\000\004\000\003\000\002\000\001\000\004\000\003\000\
\003\000\002\000\003\000\003\000\001\000\000\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\088\000\000\000\
\000\000\000\000\007\000\000\000\000\000\005\000\000\000\087\000\
\000\000\000\000\008\000\000\000\000\000\006\000\000\000\000\000\
\001\000\000\000\004\000\000\000\000\000\010\000\000\000\000\000\
\000\000\013\000\000\000\016\000\000\000\033\000\000\000\029\000\
\000\000\000\000\000\000\057\000\059\000\058\000\000\000\000\000\
\000\000\060\000\000\000\000\000\000\000\020\000\000\000\000\000\
\026\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\078\000\053\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\018\000\
\000\000\063\000\062\000\000\000\000\000\000\000\011\000\000\000\
\000\000\000\000\061\000\000\000\000\000\077\000\000\000\000\000\
\021\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\014\000\017\000\031\000\000\000\000\000\038\000\000\000\000\000\
\000\000\000\000\037\000\000\000\069\000\064\000\000\000\000\000\
\000\000\000\000\070\000\000\000\000\000\076\000\000\000\000\000\
\024\000\023\000\000\000\000\000\080\000\000\000\082\000\081\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\071\000\
\075\000\025\000\079\000\084\000\083\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\073\000\000\000\034\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\045\000\000\000\
\039\000\000\000\049\000\051\000\043\000\048\000\000\000\000\000\
\000\000\000\000\000\000\042\000\046\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\039\000\010\000\014\000\022\000\011\000\
\019\000\015\000\124\000\020\000\065\000\023\000\024\000\012\000\
\052\000\032\000\061\000\058\000\059\000\040\000\041\000\042\000\
\078\000\053\000\091\000\110\000\160\000\155\000\165\000\156\000\
\125\000\102\000\103\000\135\000"

let yysindex = "\004\000\
\072\255\000\000\006\255\014\255\016\255\031\255\000\000\013\255\
\025\255\072\255\000\000\026\255\037\255\000\000\006\255\000\000\
\051\255\049\255\000\000\016\255\053\255\000\000\031\255\057\255\
\000\000\054\255\000\000\072\255\187\000\000\000\084\255\069\255\
\038\255\000\000\031\255\000\000\038\255\000\000\058\255\000\000\
\074\255\173\255\083\255\000\000\000\000\000\000\187\000\187\000\
\187\000\000\000\234\000\087\255\122\255\000\000\031\255\095\255\
\000\000\106\255\117\255\038\255\000\000\187\000\031\255\092\255\
\130\255\000\000\000\000\133\255\000\000\054\255\187\000\187\000\
\187\000\054\255\014\255\187\000\087\255\000\000\063\255\000\000\
\244\000\000\000\000\000\187\000\187\000\187\000\000\000\187\000\
\187\000\179\000\000\000\014\255\187\000\000\000\112\255\038\255\
\000\000\062\255\000\000\199\000\136\255\120\255\142\255\038\255\
\000\000\000\000\000\000\153\255\023\001\000\000\208\000\108\255\
\134\255\205\000\000\000\187\000\000\000\000\000\151\255\021\255\
\021\255\151\255\000\000\255\000\161\255\000\000\004\001\038\255\
\000\000\000\000\038\255\038\255\000\000\031\255\000\000\000\000\
\054\255\054\255\187\000\187\000\187\000\023\001\187\000\000\000\
\000\000\000\000\000\000\000\000\000\000\232\254\135\255\023\001\
\010\255\007\001\138\255\156\255\000\000\054\255\000\000\148\255\
\000\000\187\000\054\255\054\255\150\255\187\000\000\000\187\000\
\000\000\228\000\000\000\000\000\000\000\000\000\165\000\054\255\
\054\255\152\255\232\254\000\000\000\000"

let yyrindex = "\000\000\
\155\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\155\255\000\000\000\000\000\000\000\000\005\001\000\000\
\000\000\000\000\000\000\013\001\178\255\000\000\021\001\000\000\
\000\000\185\255\000\000\155\255\000\000\000\000\000\000\254\254\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\209\255\061\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\230\255\008\000\000\000\000\000\000\000\
\000\000\000\000\177\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\000\000\243\255\
\000\000\008\255\000\000\000\000\114\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\166\255\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\042\000\110\000\
\144\000\076\000\000\000\183\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\169\255\000\000\000\000\
\139\255\185\255\000\000\000\000\000\000\054\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\170\255\000\000\088\000\
\000\000\000\000\171\255\002\255\000\000\185\255\000\000\000\000\
\000\000\000\000\163\255\185\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\185\255\
\139\255\000\000\170\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\172\000\193\000\193\255\000\000\189\000\185\000\000\000\
\190\000\000\000\227\255\000\000\244\255\000\000\229\255\226\255\
\254\255\000\000\000\000\114\000\000\000\146\000\000\000\061\000\
\000\000\180\000\147\000\000\000\046\000\055\000\000\000\000\000\
\083\000\096\000\000\000\000\000"

let yytablesize = 570
let yytable = "\051\000\
\057\000\017\000\158\000\056\000\001\000\027\000\013\000\067\000\
\034\000\027\000\112\000\084\000\085\000\086\000\016\000\034\000\
\018\000\081\000\082\000\083\000\159\000\025\000\084\000\085\000\
\068\000\088\000\089\000\095\000\047\000\047\000\066\000\021\000\
\100\000\028\000\066\000\101\000\034\000\089\000\016\000\077\000\
\034\000\108\000\109\000\111\000\162\000\034\000\114\000\099\000\
\026\000\034\000\034\000\034\000\029\000\034\000\118\000\119\000\
\120\000\066\000\121\000\122\000\062\000\031\000\021\000\127\000\
\033\000\035\000\037\000\057\000\035\000\063\000\056\000\092\000\
\113\000\150\000\151\000\038\000\093\000\064\000\060\000\035\000\
\116\000\070\000\094\000\129\000\021\000\069\000\142\000\035\000\
\035\000\126\000\080\000\136\000\004\000\066\000\167\000\054\000\
\003\000\090\000\055\000\171\000\172\000\066\000\004\000\035\000\
\096\000\035\000\101\000\005\000\006\000\152\000\153\000\154\000\
\178\000\179\000\004\000\146\000\034\000\097\000\147\000\148\000\
\055\000\128\000\074\000\034\000\098\000\066\000\104\000\074\000\
\066\000\066\000\092\000\074\000\170\000\074\000\034\000\093\000\
\154\000\105\000\175\000\034\000\106\000\094\000\034\000\034\000\
\034\000\132\000\034\000\133\000\034\000\134\000\139\000\140\000\
\084\000\034\000\084\000\085\000\086\000\034\000\034\000\034\000\
\034\000\034\000\161\000\034\000\164\000\034\000\034\000\034\000\
\088\000\089\000\034\000\034\000\144\000\016\000\166\000\169\000\
\034\000\173\000\003\000\180\000\034\000\034\000\034\000\034\000\
\034\000\034\000\137\000\052\000\022\000\034\000\034\000\034\000\
\034\000\086\000\072\000\034\000\085\000\044\000\050\000\043\000\
\034\000\071\000\027\000\030\000\034\000\072\000\034\000\036\000\
\034\000\034\000\073\000\130\000\034\000\034\000\074\000\107\000\
\075\000\034\000\076\000\168\000\174\000\079\000\034\000\115\000\
\181\000\157\000\034\000\030\000\034\000\149\000\034\000\074\000\
\074\000\074\000\000\000\030\000\030\000\074\000\074\000\074\000\
\000\000\074\000\074\000\074\000\074\000\074\000\074\000\000\000\
\074\000\074\000\054\000\030\000\000\000\030\000\000\000\074\000\
\074\000\074\000\000\000\074\000\000\000\054\000\000\000\074\000\
\074\000\056\000\056\000\056\000\000\000\054\000\054\000\056\000\
\074\000\056\000\074\000\056\000\056\000\000\000\056\000\056\000\
\056\000\000\000\056\000\055\000\000\000\054\000\000\000\054\000\
\000\000\056\000\056\000\056\000\000\000\056\000\055\000\000\000\
\000\000\056\000\056\000\000\000\065\000\065\000\055\000\055\000\
\000\000\065\000\056\000\065\000\056\000\065\000\065\000\000\000\
\065\000\065\000\065\000\000\000\065\000\036\000\055\000\000\000\
\055\000\000\000\000\000\065\000\065\000\065\000\000\000\065\000\
\036\000\000\000\000\000\065\000\065\000\000\000\066\000\066\000\
\036\000\036\000\000\000\066\000\065\000\066\000\065\000\066\000\
\066\000\000\000\066\000\066\000\066\000\000\000\066\000\041\000\
\036\000\000\000\036\000\000\000\000\000\066\000\066\000\066\000\
\000\000\066\000\041\000\000\000\000\000\066\000\066\000\000\000\
\000\000\067\000\041\000\041\000\000\000\067\000\066\000\067\000\
\066\000\067\000\067\000\000\000\067\000\067\000\000\000\000\000\
\067\000\000\000\041\000\000\000\041\000\000\000\000\000\067\000\
\067\000\067\000\000\000\067\000\000\000\000\000\000\000\067\000\
\067\000\000\000\000\000\068\000\000\000\000\000\000\000\068\000\
\067\000\068\000\067\000\068\000\068\000\000\000\068\000\068\000\
\000\000\000\000\068\000\000\000\000\000\000\000\084\000\085\000\
\086\000\068\000\068\000\068\000\000\000\068\000\000\000\000\000\
\000\000\068\000\068\000\016\000\088\000\089\000\000\000\044\000\
\045\000\046\000\068\000\016\000\068\000\047\000\123\000\044\000\
\045\000\046\000\000\000\048\000\000\000\047\000\177\000\000\000\
\084\000\085\000\086\000\048\000\000\000\000\000\084\000\085\000\
\086\000\084\000\085\000\086\000\000\000\000\000\088\000\089\000\
\000\000\049\000\000\000\050\000\088\000\089\000\000\000\088\000\
\089\000\049\000\000\000\050\000\131\000\084\000\085\000\086\000\
\000\000\138\000\141\000\084\000\085\000\086\000\000\000\000\000\
\000\000\087\000\000\000\088\000\089\000\084\000\085\000\086\000\
\000\000\088\000\089\000\000\000\000\000\176\000\000\000\117\000\
\084\000\085\000\086\000\088\000\089\000\084\000\085\000\086\000\
\084\000\085\000\086\000\143\000\000\000\000\000\088\000\089\000\
\163\000\000\000\145\000\088\000\089\000\000\000\088\000\089\000\
\084\000\085\000\086\000\000\000\009\000\009\000\000\000\000\000\
\000\000\000\000\000\000\009\000\012\000\012\000\088\000\089\000\
\009\000\009\000\000\000\012\000\015\000\015\000\000\000\000\000\
\012\000\012\000\000\000\015\000\000\000\000\000\000\000\000\000\
\015\000\015\000"

let yycheck = "\029\000\
\031\000\004\000\027\001\031\000\001\000\008\001\001\001\035\000\
\001\001\012\001\074\000\002\001\003\001\004\001\001\001\008\001\
\001\001\047\000\048\000\049\000\045\001\009\001\002\001\003\001\
\037\000\016\001\017\001\055\000\027\001\028\001\033\000\001\001\
\062\000\008\001\037\000\063\000\029\001\017\001\001\001\042\000\
\033\001\071\000\072\000\073\000\035\001\038\001\076\000\060\000\
\024\001\042\001\043\001\044\001\016\001\046\001\084\000\085\000\
\086\000\060\000\088\000\089\000\023\001\011\001\001\001\093\000\
\016\001\013\001\010\001\098\000\008\001\032\001\098\000\009\001\
\075\000\137\000\138\000\022\001\014\001\040\001\010\001\019\001\
\018\001\008\001\020\001\096\000\001\001\028\001\116\000\027\001\
\028\001\092\000\008\001\104\000\031\001\096\000\158\000\012\001\
\025\001\011\001\037\001\163\000\164\000\104\000\031\001\043\001\
\010\001\045\001\134\000\036\001\037\001\139\000\140\000\141\000\
\176\000\177\000\031\001\128\000\001\001\012\001\131\000\132\000\
\037\001\010\001\009\001\008\001\008\001\128\000\035\001\014\001\
\131\000\132\000\009\001\018\001\162\000\020\001\019\001\014\001\
\166\000\008\001\168\000\001\001\008\001\020\001\027\001\028\001\
\029\001\010\001\008\001\028\001\033\001\008\001\043\001\018\001\
\002\001\038\001\002\001\003\001\004\001\042\001\043\001\044\001\
\045\001\046\001\028\001\001\001\027\001\027\001\028\001\029\001\
\016\001\017\001\008\001\033\001\012\001\001\001\019\001\028\001\
\038\001\028\001\024\001\028\001\042\001\019\001\044\001\045\001\
\046\001\001\001\034\001\010\001\012\001\027\001\028\001\029\001\
\008\001\028\001\012\001\033\001\028\001\028\001\028\001\028\000\
\038\001\029\001\010\000\015\000\042\001\033\001\044\001\023\000\
\046\001\020\000\038\001\098\000\028\001\029\001\042\001\070\000\
\044\001\033\001\046\001\159\000\166\000\042\000\038\001\077\000\
\179\000\143\000\042\001\019\001\044\001\134\000\046\001\002\001\
\003\001\004\001\255\255\027\001\028\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\019\001\020\001\008\001\043\001\255\255\045\001\255\255\026\001\
\027\001\028\001\255\255\030\001\255\255\019\001\255\255\034\001\
\035\001\002\001\003\001\004\001\255\255\027\001\028\001\008\001\
\043\001\010\001\045\001\012\001\013\001\255\255\015\001\016\001\
\017\001\255\255\019\001\008\001\255\255\043\001\255\255\045\001\
\255\255\026\001\027\001\028\001\255\255\030\001\019\001\255\255\
\255\255\034\001\035\001\255\255\003\001\004\001\027\001\028\001\
\255\255\008\001\043\001\010\001\045\001\012\001\013\001\255\255\
\015\001\016\001\017\001\255\255\019\001\008\001\043\001\255\255\
\045\001\255\255\255\255\026\001\027\001\028\001\255\255\030\001\
\019\001\255\255\255\255\034\001\035\001\255\255\003\001\004\001\
\027\001\028\001\255\255\008\001\043\001\010\001\045\001\012\001\
\013\001\255\255\015\001\016\001\017\001\255\255\019\001\008\001\
\043\001\255\255\045\001\255\255\255\255\026\001\027\001\028\001\
\255\255\030\001\019\001\255\255\255\255\034\001\035\001\255\255\
\255\255\004\001\027\001\028\001\255\255\008\001\043\001\010\001\
\045\001\012\001\013\001\255\255\015\001\016\001\255\255\255\255\
\019\001\255\255\043\001\255\255\045\001\255\255\255\255\026\001\
\027\001\028\001\255\255\030\001\255\255\255\255\255\255\034\001\
\035\001\255\255\255\255\004\001\255\255\255\255\255\255\008\001\
\043\001\010\001\045\001\012\001\013\001\255\255\015\001\016\001\
\255\255\255\255\019\001\255\255\255\255\255\255\002\001\003\001\
\004\001\026\001\027\001\028\001\255\255\030\001\255\255\255\255\
\255\255\034\001\035\001\001\001\016\001\017\001\255\255\005\001\
\006\001\007\001\043\001\001\001\045\001\011\001\012\001\005\001\
\006\001\007\001\255\255\017\001\255\255\011\001\034\001\255\255\
\002\001\003\001\004\001\017\001\255\255\255\255\002\001\003\001\
\004\001\002\001\003\001\004\001\255\255\255\255\016\001\017\001\
\255\255\039\001\255\255\041\001\016\001\017\001\255\255\016\001\
\017\001\039\001\255\255\041\001\030\001\002\001\003\001\004\001\
\255\255\026\001\030\001\002\001\003\001\004\001\255\255\255\255\
\255\255\008\001\255\255\016\001\017\001\002\001\003\001\004\001\
\255\255\016\001\017\001\255\255\255\255\026\001\255\255\012\001\
\002\001\003\001\004\001\016\001\017\001\002\001\003\001\004\001\
\002\001\003\001\004\001\013\001\255\255\255\255\016\001\017\001\
\010\001\255\255\015\001\016\001\017\001\255\255\016\001\017\001\
\002\001\003\001\004\001\255\255\024\001\025\001\255\255\255\255\
\255\255\255\255\255\255\031\001\024\001\025\001\016\001\017\001\
\036\001\037\001\255\255\031\001\024\001\025\001\255\255\255\255\
\036\001\037\001\255\255\031\001\255\255\255\255\255\255\255\255\
\036\001\037\001"

let yynames_const = "\
  SEMI\000\
  DOT\000\
  COLON\000\
  LPAR\000\
  RPAR\000\
  COMMA\000\
  SUB\000\
  BUS\000\
  EQUAL\000\
  MINUS\000\
  ASSIGN\000\
  VBAR\000\
  ARROW\000\
  BADTOK\000\
  IMPOSSIBLE\000\
  ARRAY\000\
  BEGIN\000\
  CONST\000\
  DO\000\
  ELSE\000\
  END\000\
  IF\000\
  OF\000\
  PROC\000\
  RECORD\000\
  RETURN\000\
  THEN\000\
  TO\000\
  TYPE\000\
  VAR\000\
  WHILE\000\
  NOT\000\
  POINTER\000\
  NIL\000\
  REPEAT\000\
  UNTIL\000\
  FOR\000\
  ELSIF\000\
  CASE\000\
  "

let yynames_block = "\
  IDENT\000\
  MULOP\000\
  ADDOP\000\
  RELOP\000\
  NUMBER\000\
  CHAR\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 38 "parser.mly"
                 ( Prog (_1, ref []) )
# 432 "parser.ml"
               : Tree.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'decl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 41 "parser.mly"
                               ( makeBlock (_1, _3) )
# 440 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                   ( [] )
# 446 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decl_list) in
    Obj.repr(
# 45 "parser.mly"
                     ( _1 @ _2 )
# 454 "parser.ml"
               : 'decl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 48 "parser.mly"
                        ( _2 )
# 461 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 49 "parser.mly"
                    ( _2 )
# 468 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_decl) in
    Obj.repr(
# 50 "parser.mly"
                 ( [_1] )
# 475 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 51 "parser.mly"
                      ( [TypeDecl _2] )
# 482 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const_decl) in
    Obj.repr(
# 54 "parser.mly"
                  ( [_1] )
# 489 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'const_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'const_decls) in
    Obj.repr(
# 55 "parser.mly"
                            ( _1 :: _2 )
# 497 "parser.ml"
               : 'const_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                           ( ConstDecl (_1, _3) )
# 505 "parser.ml"
               : 'const_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_decl) in
    Obj.repr(
# 61 "parser.mly"
                 ( [_1] )
# 512 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_decls) in
    Obj.repr(
# 62 "parser.mly"
                          ( _1 :: _2 )
# 520 "parser.ml"
               : 'type_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 65 "parser.mly"
                              ( (_1, _3) )
# 528 "parser.ml"
               : 'type_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 68 "parser.mly"
                ( [_1] )
# 535 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decls) in
    Obj.repr(
# 69 "parser.mly"
                         ( _1 :: _2 )
# 543 "parser.ml"
               : 'var_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typexpr) in
    Obj.repr(
# 72 "parser.mly"
                                  ( VarDecl (VarDef, _1, _3) )
# 551 "parser.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'proc_heading) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 75 "parser.mly"
                                 ( ProcDecl (_1, _3) )
# 559 "parser.ml"
               : 'proc_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'return_type) in
    Obj.repr(
# 78 "parser.mly"
                                 ( Heading (_2, _3, _4) )
# 568 "parser.ml"
               : 'proc_heading))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                 ( [] )
# 574 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_decls) in
    Obj.repr(
# 82 "parser.mly"
                            ( _2 )
# 581 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decl) in
    Obj.repr(
# 85 "parser.mly"
                   ( [_1] )
# 588 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_decls) in
    Obj.repr(
# 86 "parser.mly"
                                  ( _1 :: _3 )
# 596 "parser.ml"
               : 'formal_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 89 "parser.mly"
                              ( VarDecl (CParamDef, _1, _3) )
# 604 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 90 "parser.mly"
                                 ( VarDecl (VParamDef, _2, _4) )
# 612 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proc_heading) in
    Obj.repr(
# 91 "parser.mly"
                   ( PParamDecl _1 )
# 619 "parser.ml"
               : 'formal_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                   ( None )
# 625 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 95 "parser.mly"
                    ( Some _2 )
# 632 "parser.ml"
               : 'return_type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 98 "parser.mly"
                 ( match _1 with [x] -> x
                                            | xs -> makeStmt (Seq _1, 0) )
# 640 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 102 "parser.mly"
            ( [_1] )
# 647 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 103 "parser.mly"
                          ( _1 :: _3 )
# 655 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'line) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt1) in
    Obj.repr(
# 106 "parser.mly"
                  ( makeStmt (_2, _1) )
# 663 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                  ( failwith "impossible" )
# 669 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                   ( !Lexer.lineno )
# 675 "parser.ml"
               : 'line))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                   ( Skip )
# 681 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                          ( Assign (_1, _3) )
# 689 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 116 "parser.mly"
                   ( ProcCall (_1, _2) )
# 697 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr_opt) in
    Obj.repr(
# 117 "parser.mly"
                      ( Return _2 )
# 704 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'elses) in
    Obj.repr(
# 118 "parser.mly"
                                 ( IfStmt (_2, _4, _5) )
# 713 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 119 "parser.mly"
                             ( WhileStmt (_2, _4) )
# 721 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                             ( RepeatStmt (_2, _4) )
# 729 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'name) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 122 "parser.mly"
                                        ( let v = makeExpr (Variable _2) in
                                          ForStmt (v, _4, _6, _8) )
# 740 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arms) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_part) in
    Obj.repr(
# 124 "parser.mly"
                                        ( CaseStmt (_2, _4, _5) )
# 749 "parser.ml"
               : 'stmt1))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
                   ( makeStmt (Skip, 0) )
# 755 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 128 "parser.mly"
                  ( _2 )
# 762 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'line) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'elses) in
    Obj.repr(
# 129 "parser.mly"
                                     ( makeStmt (IfStmt (_3, _5, _6), _2) )
# 772 "parser.ml"
               : 'elses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arm) in
    Obj.repr(
# 132 "parser.mly"
            ( [_1] )
# 779 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arms) in
    Obj.repr(
# 133 "parser.mly"
                    ( _1 :: _3 )
# 787 "parser.ml"
               : 'arms))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 136 "parser.mly"
                              ( (_1, _3) )
# 795 "parser.ml"
               : 'arm))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                   ( makeStmt (Skip, 0) )
# 801 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmts) in
    Obj.repr(
# 140 "parser.mly"
                  ( _2 )
# 808 "parser.ml"
               : 'else_part))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 143 "parser.mly"
             ( [_1] )
# 815 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Dict.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 144 "parser.mly"
                            ( _1 :: _3 )
# 823 "parser.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                   ( None )
# 829 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
            ( Some _1 )
# 836 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'variable) in
    Obj.repr(
# 151 "parser.mly"
                ( _1 )
# 843 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 152 "parser.mly"
              ( makeExpr (Constant (_1, integer)) )
# 850 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Keiko.symbol * int) in
    Obj.repr(
# 153 "parser.mly"
              ( let (lab, len) = _1 in
					  makeExpr (String (lab, len)) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 155 "parser.mly"
            ( makeExpr (Constant (int_of_char _1,
								character)) )
# 866 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
            ( makeExpr Nil )
# 872 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'name) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 158 "parser.mly"
                   ( makeExpr (FuncCall (_1, _2)) )
# 880 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 159 "parser.mly"
                ( makeExpr (Monop (Not, _2)) )
# 887 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 160 "parser.mly"
                             ( makeExpr (Monop (Uminus, _2)) )
# 894 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                      ( makeExpr (Binop (_2, _1, _3)) )
# 903 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
                      ( makeExpr (Binop (_2, _1, _3)) )
# 912 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 163 "parser.mly"
                      ( makeExpr (Binop (Minus, _1, _3)) )
# 920 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Keiko.op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 164 "parser.mly"
                      ( makeExpr (Binop (_2, _1, _3)) )
# 929 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                      ( makeExpr (Binop (Eq, _1, _3)) )
# 937 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 166 "parser.mly"
                     ( _2 )
# 944 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 169 "parser.mly"
                 ( [] )
# 950 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 170 "parser.mly"
                          ( _2 )
# 957 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
             ( [_1] )
# 964 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 174 "parser.mly"
                          ( _1 :: _3 )
# 972 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 177 "parser.mly"
            ( makeExpr (Variable _1) )
# 979 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                           ( makeExpr (Sub (_1, _3)) )
# 987 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'variable) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 179 "parser.mly"
                        ( makeExpr (Select (_1, _3)) )
# 995 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'variable) in
    Obj.repr(
# 180 "parser.mly"
                     ( makeExpr (Deref _1) )
# 1002 "parser.ml"
               : 'variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 183 "parser.mly"
            ( TypeName _1 )
# 1009 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 184 "parser.mly"
                           ( Array (_2, _4) )
# 1017 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'fields) in
    Obj.repr(
# 185 "parser.mly"
                        ( Record _2 )
# 1024 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 186 "parser.mly"
                         ( Pointer _3 )
# 1031 "parser.ml"
               : 'typexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'field_decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_semi) in
    Obj.repr(
# 189 "parser.mly"
                          ( [_1] )
# 1039 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'field_decl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 190 "parser.mly"
                            ( _1 :: _3 )
# 1047 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typexpr) in
    Obj.repr(
# 193 "parser.mly"
                              ( VarDecl (FieldDef, _1, _3) )
# 1055 "parser.ml"
               : 'field_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "parser.mly"
            ( () )
# 1061 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "parser.mly"
                   ( () )
# 1067 "parser.ml"
               : 'opt_semi))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dict.ident) in
    Obj.repr(
# 200 "parser.mly"
             ( makeName (_1, !Lexer.lineno) )
# 1074 "parser.ml"
               : 'name))
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
