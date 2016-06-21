(** tofu/boolean.ml *)
(* this module contains the code for methods of the Boolean class *)

open Lib
open Keiko

let boolean_value_offset = 4;;
let integer_value_offset = 4;;

(* push the value stored in an object whose address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; LOADW ];;
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; STOREW ];;

(* printing booleans *)
let print_code () = SEQ [
  loadLocalValue 16; (* push this.value = &Integer(x) *)
  CONST integer_value_offset;
  BINOP PlusA; LOADW;(* push 1 or 0 *)
  CONST 0;           (* static link *)
  GLOBAL "_print_num";
  PCALL 1
]

(* boolean equality *)
let isEqual_code () = Integer_methods.op2_code Eq ();;

(* operations on booleans *)
let op_code op () = let fLab = label () and exitLab = label () in SEQ [
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

let not_code () = let fLab = label () and exitLab = label () in SEQ [
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

let init_code () = SEQ [
  loadLocalValue 16;  (* push this.value = &Integer *)
  CONST 0;
  CONST integer_value_offset; BINOP PlusA; (* push value address *)
  STOREW; (* set _value._value = 0 *)
]

(* a list of method name * code pairs, to speed up pairing of method code with the method descriptors *)
let boolean_methods_code =
  [("isEqual",isEqual_code); ("print",print_code); ("not",not_code);
   ("and",and_code); ("or",or_code); ("init",init_code)]
