(** tofu/boolean.ml *)
(* this module contains the code for methods of the Boolean class *)

open Lib
open Keiko

(* push the value stored in an object whose address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; LOADW ]
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST boolean_value_offset; BINOP PlusA; STOREW ]

let isEqual_code = Integer_methods.op2_code Eq

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
  RETURNW ]          (* return address the new boolean obj *)

let and_code = op_code Times
let or_code = op_code Plus

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
]
