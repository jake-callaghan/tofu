(* tofu/object.ml *)
(* this module contains the semantic interface of the Object tofu library class *)

open Tree

(** |isEqual_desc| -- a method_desc for the isEqual method *)
let isEqual_desc = {
	method_name = "isEqual";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
	vtable_index = 0;
	locals = [];
	body = Skip;
};;

(* ... *)

(** |object_desc| -- a class_desc for the Object class *)
let object_desc = {
	class_name = "Object";
	parent_name = ""; (* object is the top of the class heirarchy *)
	parent_desc = None;
	variables = [];
	method_table = { address = -1; methods = [isEqual_desc]; };
};;

let () = (isEqual_desc.defining_class <- Some object_desc); ();;
