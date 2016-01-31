/* parser.mly */

%token<string> 		IDENT
%token<int>	  		NUMBER
/** keiko ops */
%token<Keiko.op> 	MULOP ADDOP RELOP
/** declerative */
%token 				MAIN CLASS EXTENDS NEW VAR DEF
/** statements */
%token 				WHILE IF ELSE PRINT LCURL RCURL ASSIGN RETURN
/** punctuation */
%token				DOT COMMA SEMI COLON EOF BADTOK NEWLINE
/** expressions */
%token				MINUS EQUALS LBRAC RBRAC
/** main entry point */
%start				program
%type<Tree.program>	program

%{
	open Keiko
	open Tree
%}

%%

program :
	CLASS MAIN LCURL stmt_list RCURL class_decl_list { Program (MainDecl($4), $6) };

class_decl_list :
	  /* empty */										{ [] }
	| class_decl class_decl_list						{ $1 :: $2 };

class_decl : 
	  CLASS IDENT LCURL feature_decl_list RCURL				   { ClassDecl (classDesc $2 "Object", $4) }
	| CLASS IDENT EXTENDS IDENT LCURL feature_decl_list RCURL  { ClassDecl (classDesc $2 $4, $6) };

feature_decl_list :
	  /* empty */										{ [] }
	| feature_decl feature_decl_list					{ $1 :: $2 };	 

feature_decl :
	  DEF IDENT formals COLON IDENT ASSIGN LCURL stmts RCURL { MethDecl ((methDesc $2 $5), $3, $8) } 
	| VAR IDENT COLON IDENT SEMI	{ ClassVarDecl (varDesc $2 $4) };

formals : 
	  LBRAC RBRAC					{ [] }
	| LBRAC formal_list			    { $2 };

formal_list :
	  formal 						{ [$1] }
	| formal COMMA formal_list      { $1 :: $3 };

formal :
	IDENT COLON IDENT			    { Formal ($1, $3) };

stmts :
	stmt_list					    { seq $1 };

stmt_list :
	  stmt 						    { [$1] }
	| stmt SEMI stmt_list			{ $1 :: $3 };

stmt :
	  /* empty */					{ Skip }
	| VAR IDENT COLON IDENT SEMI	{ LocalVarDecl (varDesc $2 $4) };
	| IDENT ASSIGN expr 			{ AssignStmt ($1, exprDesc $3) }
	| RETURN expr 					{ ReturnStmt (exprDesc $2) }
	| IF LBRAC expr RBRAC LCURL stmts RCURL	{ IfStmt (exprDesc $3, $6, Skip) }
	| IF LBRAC expr RBRAC LCURL stmts RCURL ELSE LCURL stmts RCURL { IfStmt(exprDesc $3, $6, $10) }
	| WHILE LBRAC expr RBRAC LCURL stmts RCURL { WhileStmt (exprDesc $3, $6) }
	| PRINT expr 					{ PrintStmt (exprDesc $2) }
	| NEWLINE 						{ NewLine };

expr : 
	  simple							{ $1 }
	| expr RELOP simple					{ Binop ($2, exprDesc $1, exprDesc $3) };

simple :
	  term 								{ $1 }
	| simple ADDOP term 				{ Binop ($2, exprDesc $1, exprDesc $3) }
	| simple MINUS term					{ Binop (Minus, exprDesc $1, exprDesc $3) }

term :
	  factor 							{ $1 }
	| term MULOP factor 				{ Binop ($2, exprDesc $1, exprDesc $3) }

factor :
	  NUMBER							{ $1 }
	| IDENT								{ Variable $1 }
	| IDENT DOT IDENT args 				{ Call ($1, $3, $4) }
	| MINUS factor						{ Monop (Uminus, exprDesc $2)}

args : 
	  LBRAC RBRAC 						{ [] }
	| LBRAC expr_list RBRAC		     	{ $2 };

expr_list :
	  expr 							{ [$1] }
	| expr COMMA expr_list 			{ $1 :: $3 };
