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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
