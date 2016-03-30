(* typecheck.ml *)

open Tree
open Env

(** |line_no| -- keeps track of line number for any errors messages *)
let line_no = ref 1

(** |annotate| -- check AST for type errors and flesh out definitions *)
let annotate (Program (mainDecl,classDecls)) = ()