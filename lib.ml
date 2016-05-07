(** /tofu/lib.ml *)
(* this module contains helper functions for the tofu library classes and misc kgen functions *)

open Tree
open Keiko
open Env

(** sizeof functions *)
let sizeof_object cdesc =
  (* sizeof storing instance variable addresses + vtable address *)
  List.fold_left (fun s vd -> s+4) 4 cdesc.variables

let sizeof_vtable vt =
  (List.length vt.methods) * 4

let sizeof_method mdesc =
  (List.length mdesc.locals) * 4

(* library class descs and functions *)
let object_desc = Object.object_desc
let integer_desc = Integer.integer_desc
let boolean_desc = Boolean.boolean_desc

let integer_value_offset = (List.find (fun vd -> vd.variable_name = "value") integer_desc.variables).offset
let boolean_value_offset = (List.find (fun vd -> vd.variable_name = "value") boolean_desc.variables).offset

let gen_object cname =
  let cdesc = find_class cname in
  let name = "%"^cdesc.class_name in
  let size = sizeof_object cdesc in
  SEQ [ CONST size; GLOBAL name; CONST 0; GLOBAL "_new"; PCALLW 2; ]

let gen_integer n =
  SEQ [ gen_object "Integer"; DUP; CONST n; CONST 0; GLOBAL "_swapTop2";
        CONST integer_value_offset; BINOP PlusA; STOREW ]

let gen_boolean b =
  let value_integer = if b then gen_integer 1 else gen_integer 0 in
  SEQ [ gen_object "Boolean"; DUP;(* 2 x new Boolean object address *)
        value_integer;            (* pushes address of new integer object *)
        CONST 0;                  (* SL *)
        GLOBAL "_swapTop2";
        CONST boolean_value_offset;
        BINOP PlusA;
        STOREW ]                   (* boolean.value <- integer *)
