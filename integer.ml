(* tofu/integer.ml *)
(* this module contains the semantic interface for the Integer tofu library class *)

open Object
open Keiko
open Tree
open Lib


(** |value| -- a variable descriptor for the state of an integer object *)
let value_desc = {
	variable_name = "_value";
	variable_type = Some "PRIMITIVE";
	variable_kind = Some Field;
	offset = 4;
};;

(* Method descriptors for the integer *)

(* this.isEqual(that : integer) : Boolean *)
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

(* this.print() *)
let print_desc = {
	method_name = "print";
	defining_class = None;
	return_type = "Unit";
	number_of_formals = 0;
	formals = [];
	vtable_index = 2;
	locals = [];
	code = NOP; body = Skip;
}

(* this.add(that : integer) : integer *)
let add_desc = {
	method_name = "add";
	defining_class = None;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 1;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.minus(that : integer) : integer *)
let minus_desc = {
	method_name = "minus";
	defining_class = None;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 2;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.multiply(that : integer) : integer *)
let multiply_desc = {
	method_name = "multiply";
	defining_class = None;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 3;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.divide(that : integer) : integer *)
let divide_desc = {
	method_name = "divide";
	defining_class = None;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 4;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.isLessThan(that : integer) : Boolean *)
let isLessThan_desc = {
	method_name = "isLessThan";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 5;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.isLessThanOrEqual(that : integer) : Boolean *)
let isLessThanOrEqual_desc = {
	method_name = "isLessThanOrEqual";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 6;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.isGreaterThan(that : integer) : Boolean *)
let isGreaterThan_desc = {
	method_name = "isGreaterThan";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 6;
	locals = [];
	code = NOP; body = Skip;
};;

(* this.isGreaterThanOrEqual(that : integer) : Boolean *)
let isGreaterThanOrEqual_desc = {
	method_name = "isGreaterThanOrEqual";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
	vtable_index = 7;
	locals = [];
	code = NOP; body = Skip;
};;

(* |integer_methods| -- a list of method descriptors on integers *)
let integer_methods = [
	isEqual_desc;
	print_desc;
	add_desc;
	minus_desc;
	multiply_desc;
	divide_desc;
	isLessThan_desc;
	isLessThanOrEqual_desc;
	isGreaterThan_desc;
	isGreaterThanOrEqual_desc;
];;

(* |integer_vtable| -- a vtable representing methods on integers *)
let integer_vtable = {
	methods = integer_methods;
};;

(* |integer_desc| -- a class_desc representing the integer type *)
let integer_desc = {
	class_name = "Integer";
	parent_name = "Object";
	parent_desc = Some object_desc;
	variables = [value_desc];
	method_table = integer_vtable;
};;

(* set defining class *)
let () = List.iter (fun md -> md.defining_class <- Some integer_desc) integer_desc.method_table.methods; ();;
(* set vtable indexes *)
let () = List.iteri (fun i md -> md.vtable_index <- i) integer_desc.method_table.methods; ();;
(* add to Lib *)
let () = Lib.add_library_class "Integer" integer_desc;;
