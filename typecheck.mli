(* typecheck.mli *)

open Tree
open Env

(** |annotate| -- check AST for type errors and flesh out definitions *)
val annotate : program -> unit

(** |SemanticError| -- raises an exception *)
exception SemanticError of string * Print.arg list * int