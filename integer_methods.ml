(** tofu/integer_methods.ml *)
(* this module contains the code for methods of the Integer class *)

open Lib
open Keiko

(* push the value stored in an object whose address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; LOADW ]
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; STOREW ]

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
]

let add_code = op1_code Plus
let minus_code = op1_code Minus
let multiply_code = op1_code Times
let divide_code = op1_code Div

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
]

let isEqual_code = op2_code Eq
let isLessThan_code = op2_code Lt
let isLessThanOrEqual_code = op2_code Leq
let isGreaterThan_code = op2_code Gt
let isGreaterThanOrEqual_code = op2_code Geq
