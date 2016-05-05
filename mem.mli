(** tofu/mem.mli *)
(* this module handles the allocation of addresses, governing the memory layout *)

open Tree

(* how many bytes does this class require? *)
val sizeof_class : class_desc -> int

(* how many bytes does this variable require? *)
val sizeof_var : variable_desc -> int

(* how many bytes does this vtable require? *)
val sizeof_vtable : vtable -> int
