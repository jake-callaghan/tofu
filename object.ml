(* tofu/object.ml *)
(* this module contains the semantic interface of the Object tofu library class *)

open Tree
open Keiko
open Lib

(********** Object methods' code **********)

(* tests if the addresses are the same *)
let isEqual_code =
  let tlab = label () and exitLab = label () in SEQ [
    LOCAL 20; LOADW;    (* push 'that' address *)
    LOCAL 16; LOADW;    (* push 'this' address *)
    JUMPC (Eq,tlab);    (* equal -> tlab *)
    gen_boolean false;  (* push addr of new Boolean(false) *)
    JUMP exitLab;
    LABEL tlab;
    gen_boolean true;   (* push addr of new Boolean(false) *)
    LABEL exitLab;
    RETURNW ]

(* calls the print primitive on the address of the object *)
let print_code = SEQ [
    LOCAL 16; LOADW;    (* push 'this' address *)
    CONST 0;            (* static link *)
    GLOBAL "_print_num";
    PCALL 1
];;

(* ... *)

(****************************************)

(** |isEqual_desc| -- a method_desc for the isEqual method *)
let isEqual_desc = {
	method_name = "isEqual";
	defining_class = None;
	return_type = "Boolean";
	number_of_formals = 1;
	formals = [Formal ("that","Object")];
	vtable_index = 0;
	locals = [];
	code = isEqual_code;
  body = Skip;
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
  code = print_code;
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
(* add to list of library class descriptors *)
let () = add_library_desc ("Object",object_desc);;
