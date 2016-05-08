(** /tofu/lib.mli *)
(* this module contains helper functions for the tofu library classes and misc kgen functions *)

open Tree
open Keiko

val sizeof_object : class_desc -> int
val sizeof_vtable : vtable -> int
val sizeof_method : method_desc -> int

(** |gen_object| -- generates code to instantiate an object of type cname *)
val gen_object : string -> code
(** |gen_integer| n -- generates code to instantiate an integer with value n *)
val gen_integer : int -> code
(** |gen_boolean| b -- generates code to instantiate a boolean with value b *)
val gen_boolean : bool -> code

val add_library_desc : (string * class_desc) -> unit
val library_descs : unit -> (string * class_desc) list

val boolean_value_offset : int
val integer_value_offset : int
