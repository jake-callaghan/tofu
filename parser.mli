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
