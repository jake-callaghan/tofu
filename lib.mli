(** /tofu/lib.mli *)
(* this module contains helper functions for the tofu library classes and misc kgen functions *)

open Tree
open Keiko

val sizeof_object : class_desc -> int
val sizeof_vtable : vtable -> int
val sizeof_method : method_desc -> int

(* library class functions and descs *)
val object_desc : class_desc
val integer_desc : class_desc
val boolean_desc : class_desc

val integer_value_offset : int
val boolean_value_offset : int

(** |gen_object| -- generates code to instantiate an object of type cname *)
val gen_object : string -> code
(** |gen_integer| n -- generates code to instantiate an integer with value n *)
val gen_integer : int -> code
(** |gen_boolean| b -- generates code to instantiate a boolean with value b *)
val gen_boolean : bool -> code
