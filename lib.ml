(** /tofu/lib.ml *)
(* this module contains helper functions for the tofu library classes and misc kgen functions *)

open Tree
open Keiko

(* library code stuff *)

let integer_value_offset = 4
let boolean_value_offset = 4

let library = ref []
let library_descs () = !library
let add_library_class cname cdesc = library := (List.append !library [(cname,cdesc)])

(** sizeof functions *)

let sizeof_object cdesc =
  (* sizeof storing instance variable addresses + vtable address *)
  List.fold_left (fun s vd -> s+4) 4 cdesc.variables

let sizeof_vtable vt =
  (List.length vt.methods) * 4

let sizeof_method mdesc =
  (List.length mdesc.locals) * 4

let gen_object cname =
  let cdesc = Env.find_class cname in
  let name = "%"^cdesc.class_name in
  let size = sizeof_object cdesc in
  SEQ [ CONST size; GLOBAL name; CONST 0; GLOBAL "_new"; PCALLW 2; ]

let gen_integer n =
  SEQ [ gen_object "Integer"; DUP; CONST n; SWAP; CONST integer_value_offset; BINOP PlusA; STOREW ]

let gen_boolean b =
  let value_integer = if b then gen_integer 1 else gen_integer 0 in
  SEQ [ gen_object "Boolean"; DUP;(* 2 x new Boolean object address *)
        value_integer;            (* pushes address of new integer object *)
        SWAP;
        CONST boolean_value_offset;
        BINOP PlusA;
        STOREW ]                   (* boolean.value <- integer *)
