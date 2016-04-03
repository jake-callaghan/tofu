(* tofu/object.ml *)
(* this module contains everything to implement the Object tofu library class *)

open Tree

(** |isEqual_desc| -- a method_desc for the isEqual method *)
let isEqual_desc = {
	method_name = "isEqual";
	defining_class = Object_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
}

(* add other method_descs here and include in Object_method_descs 
 * 
 * ...
 *
 *)

and Object_method_descs = [isEqual_desc];

(** |Object_vtable| -- a vtable representing the methods on Objects *)
and Object_vtable = {
(* address = ... ; *)
   methods = Object_method_descs;
}

(** |Object_desc| -- a class_desc for the Object class *)
and Object_desc = {
	class_name = "Object";
	parent_name = ""; (* object is the top of the class heirarchy *)
	variables = [];
	vtable = Object_vtable;
}