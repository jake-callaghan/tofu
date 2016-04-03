(* tofu/integer.ml *)
(* this module contains everything to implement the Integer tofu library class *)

open Object
open Tree

(** |value|| -- a variable descriptor for the state of an Integer object *)
let value_desc = {
	variable_name = "value";
	variable_type = "primitve" (* 'primitive' denotes a special case of types to be handled later on *)
}

(* Method descriptors for the Integer *)

(* this.isEqual(that : Integer) : Boolean *)
and isEqual_desc = {
	method_name = "equals";
	defining_class = ref Integer_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
}

(* this.add(that : Integer) : Integer *)
and add_desc = {
	method_name = "add";
	defining_class = ref Integer_desc;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.minus(that : Integer) : Integer *)
and minus_desc = {
	method_name = "minus";
	defining_class = ref Integer_desc;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.multiply(that : Integer) : Integer *)
and multiply_desc = {
	method_name = "multiply";
	defining_class = ref Integer_desc;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.divide(that : Integer) : Integer *)
and divide_desc = {
	method_name = "divide";
	defining_class = ref Integer_desc;
	return_type = "Integer";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.isLessThan(that : Integer) : Boolean *)
and isLessThan_desc = {
	method_name = "isLessThan";
	defining_class = ref Integer_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.isLessThanOrEqual(that : Integer) : Boolean *)
and isLessThanOrEqual_desc = {
	method_name = "isLessThanOrEqual";
	defining_class = ref Integer_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.isGreaterThan(that : Integer) : Boolean *)
and isGreaterThan_desc = {
	method_name = "isGreaterThan";
	defining_class = ref Integer_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* this.isGreaterThanOrEqual(that : Integer) : Boolean *)
and isGreaterThanOrEqual_desc = {
	method_name = "isGreaterThanOrEqual";
	defining_class = ref Integer_desc;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Integer")];
}

(* |Integer_methods| -- a list of method descriptors on Integers *)
and Integer_methods = [
	isEqual_desc,
	add_desc,
	minus_desc,
	multiply_desc,
	divide_desc,
	isLessThan_desc,
	isLessThanOrEqual_desc,
	isGreaterThan_desc,
	isGreaterThanOrEqual_desc
];

(* |Integer_vtable| -- a vtable representing methods on Integers *)
let Integer_vtable = {
 (* address = ... ; *)
	methods = Integer_method_descs;
}
 
(* |Integer_desc| -- a class_desc representing the Integer type *)
and Integer_desc = {
	class_name = "Integer";
	parent_name = "Object";
	parent_desc = Object_desc;
	variables = [value_desc];
	method_table = Integer_vtable;
}