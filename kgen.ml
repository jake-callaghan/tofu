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

(** |gen_var_addr| -- generate code to push a variable's address onto stack *)
let gen_var_addr mdesc vdesc = match unwrap vdesc.var_kind with
  | Field -> SEQ [push_this; CONST offset; BINOP PlusA; LOADW]
  | Local | Arg -> SEQ [LOCAL offset; LOADW]

(** |gen_method_call| -- generate code to call a method, rw = true if a word is returned *)
let gen_method_call edesc2 mname arg_edescs rw =
  let callOp = if rw then PCALLW else PCALL in
  let cdesc = find_class cname in
  let mdesc = find_method cdesc mname in SEQ [
    List.rev_map (gen_expr mdesc) arg_edescs; (* push arg addrs and reverse the list *)
    (*---calculate method's address---*)
    gen_expr mdesc edesc2;          (* var address *)
    DUP;                            (* 2x var address: 1 for lookup, 1 passed as argument *)
    LOADW;                          (* push vtable address *)
    CONST (4*mdesc.vtable_index);    (* offset in vtable *)
    BINOP PlusA; LOADW;             (* push method address *)
    (*--------------------------------*)
    callOp (List.size arg_edescs)+1 (* n args + variable's address *)
  ]

(** |gen_expr| -- generate code for expressions *)
and gen_expr mdesc edesc = match edesc.expr_guts with
  | This -> push_this
  | Number n  -> gen_integer n
  | Boolean b -> gen_boolean b
  | Variable vdesc -> gen_var_addr mdesc
  | NewObject cname -> gen_object cname
  | Call (edesc2,mname,arg_edescs) -> gen_method_call edesc2 mname arg_edescs true

(** |gen_stmt| -- generate code for statements *)
and gen_stmt mdesc body = match body with
    Skip -> NOP
  | Seq ss -> List.iter (gen_stmt mdesc) ss
  | UnitCall (edesc,mname,arg_edescs) -> gen_method_call edesc mname arg_edescs false
  | LocalVarDecl (vdesc,cname) -> SEQ [gen_object cname; LOCAL vdesc.offset; STOREW ]
  | AssignStmt (vname,edesc) -> 
  | ReturnStmt edesc -> SEQ [gen_expr mdesc edesc; RETURNW ]
  | IfStmt (edesc,tss,fss) ->
      let tlab = label () and flab = label () and elab = label () in
      SEQ [ gen_cond mdesc edesc tlab flab;
            LABEL tlab; gen_stmt mdesc tss; JUMP elab;
            LABEL flab; gen_stmt mdesc fss; LABEL elab ]
  | WhileStmt (edesc,ss) ->
      let startlab = label () and bodylab = label () and exitlab = label () in
      SEQ [ LABEL start;
            gen_cond mdesc desc bodylab exitlab;
            LABEL bodylab; gen_stmt mdesc ss; JUMP start;
            LABEL exitlab ]
  | PrintStmt edesc -> []
  | Newline -> NOP

(** |gen_cond| -- gen code to jump to tlab if texpr is true, flab if false *)
and gen_cond mdesc edesc tlab flab = SEQ [

]
