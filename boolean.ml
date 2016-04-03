(* tofu/boolean.ml *)
(* this module contains everything to implement the Boolean tofu library class *)

open Tree

(** |value_desc| -- a variable descriptor that contains the state of a Boolean object *)
let value_desc = {
	variable_name = "value";
	variable_type = "Integer";
}

(** |isEqual_desc| -- a method_desc for the isEqual method *)
and isEqual_desc = {
	method_name = "isEqual";
	defining_class = Boolean_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
}

(** |not_desc| -- a method_desc for the not method *)
and not_desc = {
	method_name = "not";
	defining_class = Boolean_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Boolean")];
}

(** |and_desc| -- a method_desc for the and method *)
and and_desc = {
	method_name = "and";
	defining_class = Boolean_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Boolean")];
}

(** |or_desc| -- a method_desc for the or method *)
and or_desc = {
	method_name = "or";
	defining_class = Boolean_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Boolean")];
}

and Boolean_method_descs = [isEqual_desc, not_desc, and_desc, or_desc];

(** |Object_vtable| -- a vtable representing the methods on Objects *)
and Object_vtable = {
(* address = ... ; *)
   methods = Object_method_descs;
}

(** |Object_desc| -- a class_desc for the Object class *)
and Object_desc = {
	class_name = "Object";
	parent_name = ""; (* object is the top of the class heirarchy *)
	variables = [value_desc];
	vtable = Object_vtable;
}