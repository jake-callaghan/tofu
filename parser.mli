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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Tree.program
