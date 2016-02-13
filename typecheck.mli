(* typecheck.mli *)

open Tree
open Env

(* |explicate| -- check AST for type erros and flesh out definitios *)
val explicate : program -> unit

(* |SemanticError| -- raises an exception *)
exception SemanticError of string * Print.arg list * int