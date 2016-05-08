(* tofu/integer.ml *)
(* this module contains the semantic interface for the Integer tofu library class *)

open Object
open Keiko
open Tree
open Lib

(******* Integer methods' code *********)

let integer_value_offset = 4;;

(* push the value stored in an object whose address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; LOADW ];;
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; STOREW ];;

(* arithmetic on integers *)

let op1_code op = SEQ [
  gen_object "Integer"; (* push 2 x new Integer addr *)
  DUP;
  loadLocalValue 16;    (* push this.value *)
  loadLocalValue 20;    (* push that.value *)
  BINOP op;             (* pop values and push the op applied to them *)
  CONST 0; GLOBAL "_swapTop2";
  CONST integer_value_offset;
  BINOP PlusA; STOREW;  (* store value in the new integer *)
  RETURNW               (* return address of new integer *)
];;

let add_code = op1_code Plus;;
let minus_code = op1_code Minus;;
let multiply_code = op1_code Times;;
let divide_code = op1_code Div;;

(* comparison of integers *)

let op2_code op =  SEQ [
  let tlab = label () and exitLab = label () in SEQ [
    loadLocalValue 16;  (* push this.value *)
    loadLocalValue 20;  (* push that.value *)
    JUMPC (op,tlab);    (* equal values -> tlab *)
    gen_boolean false;  (* push new Boolean(false) *)
    JUMP exitLab;
    LABEL tlab;
    gen_boolean true;   (* push new Boolean(true) *)
    LABEL exitLab;
    RETURNW ]
];;

let isEqual_code = op2_code Eq;;
let isLessThan_code = op2_code Lt;;
let isLessThanOrEqual_code = op2_code Leq;;
let isGreaterThan_code = op2_code Gt;;
let isGreaterThanOrEqual_code = op2_code Geq;;

(* printing integers *)
let print_code = SEQ [
  loadLocalValue 16;  (* push this.value *)
  CONST 0;            (* static link *)
  GLOBAL "_print_num";(* push prim address *)
  PCALL 1;
];;

(* ... *)

(**************************************)

(** |value|| -- a variable descriptor for the state of an integer object *)
let value_desc = {
	variable_name = "value";
	variable_type = Some "primitve"; (* 'primitive' denotes a special case of types to be handled later on *)
	variable_kind = Some Local;
	offset = 4;
};;

(* Method descriptors for the integer *)

(* this.isEqual(that : integer) : Boolean *)
let isEqual_desc = {
	method_name = "equals";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
	vtable_index = 0;
	locals = [];
  body = Skip;
	code = isEqual_code;
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
	code = print_code; body = Skip;
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
	code = add_code; body = Skip;
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
	code = minus_code; body = Skip;
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
	code = multiply_code; body = Skip;
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
	code = divide_code; body = Skip;
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
	code = isLessThan_code; body = Skip;
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
	code = isLessThanOrEqual_code; body = Skip;
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
	code = isGreaterThan_code; body = Skip;
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
	code = isGreaterThanOrEqual_code; body = Skip;
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
	address = -1;
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
(* add to the list of library descriptors *)
let () = add_library_desc ("Integer",integer_desc);;
