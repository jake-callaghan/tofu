{
	open Parser (* token type is defined in parser.mli *)
	exception Eof
}
rule token = parse 
	  [' ' '\t']			{token lexbuf}
	| ['\n']				{ EOL }
	| ['0' - '9']+ as lxm   { INT(int_of_string lxm) }
	| '+'					{ PLUS }
	| '-'					{ MINUS }
	| '*'					{ TIMES }
	| '/'					{ DIV }
	| '('					{ LPAREN }
    | ')' 					{ RPAREN }
    | eof 					{ raise Eof }