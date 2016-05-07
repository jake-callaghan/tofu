(** tofu/kgen.ml *)
(* this module generates code for various constructs in the ASTs *)

open Env
open Lib
open Tree
open Keiko

(* |translate| -- generate code for the whole program *)
let translate p = ()

(* push the address of this object *)
let push_this = SEQ [LOCAL 16; LOADW]

(** |gen_var_addr| -- generate code to push variable v's address onto stack *)
let gen_var_addr mdesc vname =
  let cdesc = unwrap mdesc.defining_class in
  let vdesc = find_variable cdesc mdesc vname in
  match unwrap vdesc.var_kind with
        | Field -> SEQ [push_this; CONST offset; BINOP PlusA; LOADW]
        | Local | Arg -> SEQ [LOCAL offset; LOADW]

(** |gen_expr| -- generate code for expressions *)
let rec gen_expr mdesc edesc = match edesc.expr_guts with
| Number n  -> gen_integer n
| Boolean b -> gen_boolean b
| Variable vdesc ->
    let offset = vdesc.offset in
    let cdesc = find_class (unwrap vdesc.variable_type)
| NewObject cname -> gen_object cname
| Call (edesc2,mname,arg_edescs) ->
    SEQ [ pu
          gen_expr mdesc edesc2;   (* push object's address *)
          LOADW;                   (* push object's vtable address *)

         ]
