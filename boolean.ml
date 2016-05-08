(* tofu/boolean.ml *)
(* this module contains the semantic interface of the Boolean tofu library class *)

open Tree
open Object
open Keiko
open Lib

(************ Boolean methods' code **********)

let boolean_value_offset = 4;;
let integer_value_offset = Integer.integer_value_offset;;

(* push the value stored in an object whose address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; LOADW ];;
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; STOREW ];;

let isEqual_code = Integer.op2_code Eq;;

let print_code = SEQ [
	loadLocalValue 16;																(* push value address *)
	CONST integer_value_offset; BINOP PlusA; LOADW;   (* push integer.value *)
	CONST 0;								(* static link *)
	GLOBAL "_print_num";		(* push prim addr *)
	PCALL 1;								(* call prim /w the integer value as argument *)
];;

let op_code op = let fLab = label () and exitLab = label () in SEQ [
  loadLocalValue 16;  (* push this.value = &Integer(x) onto stack *)
  CONST integer_value_offset; BINOP PlusA; LOADW; (* push value *)
  loadLocalValue 20;  (* push that.value = &Integer(x) onto stack *)
  CONST integer_value_offset; BINOP PlusA; LOADW; (* push value *)
  BINOP op;
  CONST 0;
  JUMPC (Eq,fLab);          (* 0 -> false *)
  gen_boolean true;
  JUMP exitLab;
  LABEL fLab;
  gen_boolean false;
  LABEL exitLab;
  RETURNW ];;               (* return address the new boolean obj *)

let and_code = op_code Times;;
let or_code = op_code Plus;;

let not_code = let fLab = label () and exitLab = label () in SEQ [
  loadLocalValue 16;  (* push this.value = &Integer(x) onto stack *)
  CONST integer_value_offset; BINOP PlusA; LOADW; (* push value *)
  CONST 0;
  JUMPC (Eq,fLab);
  gen_boolean false;
  JUMP exitLab;
  LABEL fLab;
  gen_boolean true;
  LABEL exitLab;
  RETURNW;
];;

(* ... *)

(*********************************************)

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
	code = isEqual_code; body = Skip;
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
	code = print_code; body = Skip;
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
	code = not_code; body = Skip;
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
	code = and_code; body = Skip;
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
	code = or_code; body = Skip;
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
(* add to the list of library class descriptors *)
let () = add_library_desc ("Boolean",boolean_desc);;
