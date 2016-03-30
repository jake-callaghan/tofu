(* tofu/errors.ml *)

(* a module to define any error / exceptions used throughout compilation *)

open Print

(** |SemanticError| -- represents a semantic error exception *)
exception SemanticError of string * Print.arg list * int
let semanticError msg args line = 
	raise (SemanticError (msg,args,line))

(** |ClassNameError| -- represents a problem with a particular class name *)
exception ClassNameError of string 
let classNameError msg  = 
	raise (ClassNameError msg)
