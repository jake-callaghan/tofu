/* parser.mly */

%token<string> 		IDENT
%token<int>	  		NUMBER
%token 						TRUE FALSE
%token						THIS
/** decleratives */
%token 						MAIN CLASS EXTENDS NEW VAR DEF
/** statements */
%token 						WHILE IF ELSE LCURL RCURL ASSIGN RETURN
/** punctuation */
%token						DOT COMMA SEMI COLON EOF BADTOK NEWLINE LBRAC RBRAC
/** main entry point */
%start				program
%type<Tree.program>	program

%{
	open Tree
	open Keiko
%}

%%

program :
	MAIN LCURL stmts RCURL class_decl_list { Program ( mainDesc (methodDesc "main" "-" [] $3)  , $5 ) };

/********* Declerations **********/

class_decl_list :
	  /* empty */										{ [] }
	| class_decl class_decl_list		{ $1 :: $2 };

class_decl :
	  CLASS IDENT LCURL feature_decl_list RCURL				  			 { ClassDecl (classDesc $2 "Object", $4) }
	| CLASS IDENT EXTENDS IDENT LCURL feature_decl_list RCURL  { ClassDecl (classDesc $2 $4, $6) };

feature_decl_list :
	  /* empty */											{ [] }
	| feature_decl feature_decl_list	{ $1 :: $2 };

feature_decl :
	  DEF IDENT formals COLON IDENT ASSIGN LCURL stmts RCURL { MethDecl (methodDesc $2 $5 $3 $8) }
	| VAR IDENT COLON IDENT SEMI														 { InstanceVarDecl ((variableDesc $2),$4) };

formals :
	  LBRAC RBRAC										{ [] }
	| LBRAC formal_list	RBRAC	    	{ $2 };

formal_list :
	  formal 												{ [$1] }
	| formal COMMA formal_list      { $1 :: $3 };

formal :
	IDENT COLON IDENT			    			{ Formal ($1, $3) };

/*********** Statements ************/

stmts :
	stmt_list					    		{ seq $1 };

stmt_list :
	  stmt 						    		{ [$1] }
	| stmt SEMI stmt_list			{ $1 :: $3 };

stmt :
	  /* empty */																												{ Skip }
	| VAR IDENT COLON IDENT     																				{ LocalVarDecl ((variableDesc $2),$4) }
	| IDENT ASSIGN expr      																						{ AssignStmt ((variableDesc $1), $3) }
	| RETURN expr        																								{ ReturnStmt $2 }
	| IF LBRAC expr RBRAC LCURL stmts RCURL															{ IfStmt ($3, $6, Skip) }
	| IF LBRAC expr RBRAC LCURL stmts RCURL ELSE LCURL stmts RCURL 			{ IfStmt($3, $6, $10) }
	| WHILE LBRAC expr RBRAC LCURL stmts RCURL 													{ WhileStmt ($3, $6) }
	| NEWLINE 																													{ Newline }
	| expr DOT IDENT args 																							{ UnitCall ($1, $3, $4) };
	| IDENT args																												{ UnitCall (exprDesc This, $1, $2)}  /* implicit this.method(args) call */

/*********** Expressions ************/

expr :
	| THIS 									{ exprDesc This }
	| NUMBER 								{ exprDesc (Number $1)  }
	|	TRUE						 			{ exprDesc (Boolean true) }
	| FALSE							  	{ exprDesc (Boolean false) }
	| IDENT							   	{ exprDesc (Variable (variableDesc $1)) }
	| NEW IDENT 						{ exprDesc (NewObject $2) }
	| expr DOT IDENT args	  { exprDesc (Call ($1, $3, $4)) };
	| IDENT args						{ exprDesc (Call (exprDesc This, $1, $2))}	/* implicit this.method(args) call */

args :
	  LBRAC RBRAC 									{ [] }
	| LBRAC expr_list RBRAC		     	{ $2 };

expr_list :
	  expr 											{ [$1] }
	| expr COMMA expr_list 			{ $1 :: $3 };
