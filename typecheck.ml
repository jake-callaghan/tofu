(* typecheck.ml *)

open Tree
open Env
open Print

(** |line_no| -- keeps track of line number for any errors messages *)
let line_no = ref 1

(** |annotate| -- check AST for type errors and flesh out definitions *)
let annotate (Program (mainDecl,classDecls)) = ()

(** |SemanticError| -- represents a semantic error exception *)
exception SemanticError of string * Print.arg list * int

(** |semError| -- raises a sematnic error exception *)
let semError msg args = 
	raise (SemanticError (msg,args,!line_no))