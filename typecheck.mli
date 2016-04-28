(* tofu/typecheck.mli *)

open Tree
open Env

(** |annotate| -- check AST for type errors and annotate descriptors *)
val annotate : program -> bool -> unit