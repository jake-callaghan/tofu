(* tofu/errors.ml *)

(* a module to define any error / exceptions used throughout compilation *)

open Print

(** |CompilationError| -- represents an compiler error *)
exception CompilationError of string
let compilationError msg =
	raise (CompilationError msg);;

(** |SemanticError| -- represents a semantic error exception *)
exception SemanticError of string
let semanticError msg =
	raise (SemanticError msg);;

(** |ClassNameError| -- represents a problem with a particular class name *)
exception ClassNameError of string
let classNameError msg  =
	raise (ClassNameError msg);;

(** |MethodNameError| -- represents a problem with a particular method name *)
exception MethodNameError of string
let methodNameError msg =
	raise (MethodNameError msg);;

(** |VariableNameError| -- represents a problem with a particular variable name *)
exception VariableNameError of string
let variableNameError msg =
	raise (VariableNameError msg);;

(** |ArgumentError| -- represents a problem with args and a method signature *)
exception ArgumentError of string
let argumentError msg =
	raise (ArgumentError msg);;
