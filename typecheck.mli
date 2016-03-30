(* typecheck.mli *)

open Tree
open Env

(** |annotate| -- check AST for type errors and flesh out definitions *)
val annotate : program -> unit