(* tofu/boolean.ml *)
(* this module contains the semantic interface of the Boolean tofu library class *)

open Tree
open Object
open Keiko
open Lib

(** |value_desc| -- a variable descriptor that contains the state of a Boolean object *)
let value_desc = {
	variable_name = "value";
	variable_type = Some "Integer";
	variable_kind = Some Local;
	offset = 4;
}

(** |isEqual_desc| -- a method_desc for the isEqual method *)
let isEqual_desc = {
	method_name = "isEqual";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
	vtable_index = 0;
	locals = [];
	code = NOP; body = Skip;
};;

(** |print| -- calls the primitive print *)
let print_desc = {
	method_name = "print";
	defining_class = None;
	return_type = "Unit";
	number_of_formals = 0;
	formals = [];
	vtable_index = 2;
	locals = [];
	code = NOP; body = Skip;
};;

(** |not_desc| -- a method_desc for the not method *)
let not_desc = {
	method_name = "not";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 0;
	formals = [];
	vtable_index = 1;
	locals = [];
	code = NOP; body = Skip;
};;

(** |and_desc| -- a method_desc for the and method *)
let and_desc = {
	method_name = "and";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Boolean")];
	vtable_index = 2;
	locals = [];
	code = NOP; body = Skip;
};;

(** |or_desc| -- a method_desc for the or method *)
let or_desc = {
	method_name = "or";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Boolean")];
	vtable_index = 3;
	locals = [];
	code = NOP; body = Skip;
};;

let boolean_mds = [isEqual_desc; print_desc; not_desc; and_desc; or_desc];;

(** |Boolean_vtable| -- a vtable representing the methods on Booleans *)
let boolean_vtable = {
   address = -1;
   methods = boolean_mds;
};;

(** |Boolean_desc| -- a class_desc for the Boolean type *)
let boolean_desc = {
	class_name = "Boolean";
	parent_name = "Object";
	parent_desc = Some object_desc;
	variables = [value_desc];
	method_table = boolean_vtable;
};;

(* asign defining class *)
let () = List.iter (fun md -> md.defining_class <- Some boolean_desc) boolean_desc.method_table.methods; ();;
(* set vtable indexes *)
let () = List.iteri (fun i md -> md.vtable_index <- i) boolean_desc.method_table.methods; ();;
(* add to the Lib *)
let () = Lib.add_library_class "Boolean" boolean_desc;;
