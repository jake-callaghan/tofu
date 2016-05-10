(* tofu/object.ml *)
(* this module contains the semantic interface of the Object tofu library class *)

open Tree
open Keiko
open Lib

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
  code = NOP;
};;

(** |print| -- calls the primitive print *)
let print_desc = {
	method_name = "print";
	defining_class = None;
	return_type = "Unit";
	number_of_formals = 0;
	formals = [];
	vtable_index = 1;
	locals = [];
  body = Skip;
  code = NOP;
};;

(* ... *)

(** |object_desc| -- a class_desc for the Object class *)
let object_desc = {
	class_name = "Object";
	parent_name = ""; (* object is the top of the class heirarchy *)
	parent_desc = None;
	variables = [];
	method_table = { address = -1; methods = [isEqual_desc; print_desc]; };
};;

(* set the defining class for all method_descriptors *)
let () = List.iter (fun md -> md.defining_class <- Some object_desc) object_desc.method_table.methods; ();;
(* add this to the Lib *)
let () = Lib.add_library_class "Object" object_desc;;
