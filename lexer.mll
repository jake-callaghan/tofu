(* lexer.mll *)

{
	open Keiko
	open Parser
	open Tree
	open Lexing
	exception Eof

let lineno = ref 1

}

rule token = parse
	  [' ' '\t']		{token lexbuf}
	| ['0'-'9']+ as s   { NUMBER (int_of_string s) }
	| "main"			{ MAIN }
	| "Main"			{ MAIN }
	| "class"			{ CLASS }
	| "extends"			{ EXTENDS }
	| "var"				{ VAR }
	| "def"				{ DEF }
	| "return" 			{ RETURN }
	| "print" 			{ PRINT }
	| "new"				{ NEW }
	| "while"			{ WHILE }
	| "if"				{ IF }
	| "else"			{ ELSE }
	| "."				{ DOT }
	| ","				{ COMMNA }
	| "{"				{ LCURL }
	| "}"				{ RCURL }
	| ";"				{ SEMI }
	| ":"				{ COLON }
	| "("				{ LBRAC }
	| ")"				{ RBRAC }
	| "="				{ ASSIGN }
	| "=="				{ RELOP Eq }
	| "+"				{ ADDOP Plus }
	| "-"				{ MINUS }
	| "*"				{ MULOP Times }
	| "<"				{ RELOP Lt }
	| ">"				{ RELOP Gt }
	| "<>"				{ RELOP Neq }
	| "<="				{ RELOP Leq }
	| ">="				{ RELOP Geq }
	| "(*"				{ comment lexbuf; token lexbuf }
	| "\n"				{ incr lineno; Source.note_line !lineno lexbuf; token lexbuf }
	| eof 				{ EOF }
	| _ 				{ BADTOK }

and comment = parse 
	  "*)"				{ () }
	| "\n" 				{ incr lineno; Source.note_line; !lineno lexbuf; comment lexbuf }
	| _					{ comment lexbuf }
	| eof 				{ () }	

