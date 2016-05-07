(* tofu/env.mli *)

open Tree

type environment

val library_descs : (string * class_desc) list

val size : unit -> int
val print: unit -> unit
val unwrap : 'a option -> 'a

val add_class : class_desc -> feature_decl list -> unit
val find_class : string -> class_desc
val is_subclass : string -> string -> bool

val add_method : class_desc -> method_desc -> unit
val find_method : class_desc -> string -> method_desc


val find_instance_var : class_desc -> string -> variable_desc
