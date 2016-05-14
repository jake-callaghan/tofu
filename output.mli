(** tofu/output.mli *)
(* this module provides methods for outputting Keiko code along with assembler directives *)

(** |output| -- write the compiler's output to stdout *)
val output : Tree.program -> unit
