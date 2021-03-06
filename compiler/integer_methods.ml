(** tofu/integer_methods.ml *)
(* this module contains the code for methods of the Integer class *)

open Lib
open Keiko

(* push the value stored in an object whose object's address is at offset o *)
let loadLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; LOADW ]
(* store the value on stack in object.value, where object address is at offset o *)
let storeLocalValue o =
  SEQ [LOCAL o; LOADW; CONST integer_value_offset; BINOP PlusA; STOREW ]

(* printing integers *)
let print_code () = SEQ [
  loadLocalValue 16; (* push this.value *)
  CONST 0;           (* static link *)
  GLOBAL "_print_num";
  PCALL 1
]

(* arithmetic on integers *)

let op1_code op () = SEQ [
  gen_object "Integer"; (* push 2 x new Integer addr *)
  DUP;
  loadLocalValue 16;    (* push this.value *)
  loadLocalValue 20;    (* push that.value *)
  BINOP op;             (* pop values and push the v1+v2 *)
  SWAP;
  CONST integer_value_offset;
  BINOP PlusA; STOREW;  (* store value in the new integer *)
  RETURNW               (* return address of new integer *)
]

let add_code = op1_code Plus
let minus_code = op1_code Minus
let multiply_code = op1_code Times
let divide_code = op1_code Div

(* comparison of integers *)

let op2_code op () =  SEQ [
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

let init_code () = SEQ [
  CONST 0;          (* push 0 *)
  LOCAL 16; LOADW;  (* push this addr *)
  CONST integer_value_offset; BINOP PlusA; (* push this._value addr *)
  STOREW;   (* this._value = 0 *)
]

(* a list of method name * code pairs, to speed up pairing of method code with the method descriptors *)
let integer_methods_code =
  [("isEqual",isEqual_code); ("print",print_code);
    ("add",add_code); ("minus",minus_code); ("multiply",multiply_code); ("divide",divide_code);
    ("isLessThan",isLessThan_code); ("isLessThanOrEqual",isLessThanOrEqual_code);
    ("isGreaterThan",isGreaterThan_code); ("isGreaterThanOrEqual",isGreaterThan_code);
    ("init",init_code)]
