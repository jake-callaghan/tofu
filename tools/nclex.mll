(* nclex.mll *)

{
open Ncparse 
open String 
open Lexing

let line_no = ref 1
}

rule token =
  parse
      "<"(['A'-'Z''a'-'z']+|'('[^')']+')' as s)  { OPEN s }
    | "?<"(['A'-'Z''a'-'z']+|'('[^')']+')' as s)  { POPEN s }
    | [^'<''>''('')'',''@'' ''\n']+ as s  { WORD s }
    | "(*"[^'\n']*"*)" as s     { WORD s }
    | " "                       { SPACE }
    | ">"                       { CLOSE }
    | "("                       { LPAREN }
    | ")"                       { RPAREN }
    | ","                       { COMMA }
    | "@"                       { ATSIGN }
    | "\n"                      { incr line_no; CHAR '\n' }
    | _ as c                    { CHAR c }
    | eof                       { EOF }

