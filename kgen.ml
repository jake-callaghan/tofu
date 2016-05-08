(** tofu/kgen.ml *)
(* this module generates code for various constructs in the ASTs *)

open Env
open Lib
open Tree
open Keiko

(* push the address of this object *)
let push_this = SEQ [LOCAL 16; LOADW]

(** |gen_var_addr| -- generate code to push a variable's address onto stack *)
let gen_var_addr mdesc vdesc = match unwrap vdesc.variable_kind with
  | Field -> SEQ [push_this; CONST vdesc.offset; BINOP PlusA; LOADW]
  | Local | Arg -> SEQ [LOCAL vdesc.offset; LOADW]

(** |gen_method_call| -- generate code for "edesc2.mname(arg_edescs...)" where rw = true if a word is returned *)
let rec gen_method_call mdesc edesc2 mname arg_edescs rw =
  let argsize = (List.length arg_edescs)+1 in
  let callOp = if rw then PCALLW argsize else PCALL argsize in
  let gen_args = SEQ (List.rev_map (gen_expr mdesc) arg_edescs) in
  let cdesc = find_class (unwrap edesc2.expr_type) in
  let mdesc2 = find_method cdesc mname in (* the descriptor of the method being called *)
  SEQ [
    gen_args;
    (*---calculate method's address---*)
    gen_expr mdesc edesc2;          (* var address *)
    DUP;                            (* 2x var address: 1 for lookup, 1 passed as argument *)
    LOADW;                          (* push vtable address *)
    CONST (4*mdesc2.vtable_index);   (* offset in vtable *)
    BINOP PlusA; LOADW;             (* push method address *)
    (*--------------------------------*)
    callOp ]                        (* n args + variable's address *)

(** |gen_expr| -- generate code for expressions *)
and gen_expr mdesc edesc = match edesc.expr_guts with
  | This -> push_this
  | Number n  -> gen_integer n
  | Boolean b -> gen_boolean b
  | Variable vdesc -> gen_var_addr mdesc vdesc
  | NewObject cname -> gen_object cname
  | Call (edesc2,mname,arg_edescs) -> gen_method_call mdesc edesc2 mname arg_edescs true

(** |gen_stmt| -- generate code for statements *)
and gen_stmt mdesc body = match body with
    Skip -> NOP
  | Seq ss -> SEQ (List.map (gen_stmt mdesc) ss)
  | UnitCall (edesc,mname,arg_edescs) -> gen_method_call mdesc edesc mname arg_edescs false
  | LocalVarDecl (vdesc,cname) -> SEQ [gen_object cname; LOCAL vdesc.offset; STOREW ]
  | AssignStmt (vdesc,edesc) -> SEQ [gen_expr mdesc edesc; gen_var_addr mdesc vdesc; STOREW ]
  | ReturnStmt edesc -> SEQ [gen_expr mdesc edesc; RETURNW ]
  | IfStmt (edesc,tss,fss) ->
      let tlab = label () and flab = label () and elab = label () in
      SEQ [ gen_cond mdesc edesc tlab flab;
            LABEL tlab; gen_stmt mdesc tss; JUMP elab;
            LABEL flab; gen_stmt mdesc fss; LABEL elab ]
  | WhileStmt (edesc,ss) ->
      let startlab = label () and bodylab = label () and exitlab = label () in
      SEQ [ LABEL startlab;
            gen_cond mdesc edesc bodylab exitlab;
            LABEL bodylab; gen_stmt mdesc ss; JUMP startlab;
            LABEL exitlab ]
  | Newline -> NOP

(** |gen_cond| -- gen code to jump to tlab if texpr is true, flab if false *)
and gen_cond mdesc edesc tlab flab = SEQ [
  gen_expr mdesc edesc;     (* push Boolean expr object's addr onto stack *)
  CONST Lib.boolean_value_offset;
  BINOP PlusA; LOADW;
  CONST Lib.integer_value_offset;
  BINOP PlusA; LOADW;       (* push x = 1 or x = 0 onto stack *)
  CONST 0;                  (* x = 0 -> jump to false label, otherwise true label *)
  JUMPC (Eq,flab);
  JUMP tlab;
]

(** |gen_method| -- gen code for a method's body *)
let gen_method mdesc = mdesc.code <- Keiko.canon (gen_stmt mdesc mdesc.body)
(** |gen_vtable| -- gen code for all methods in the vtable *)
let gen_vtable vt = List.iter gen_method vt.methods
(** |gen_class| -- gen code for a class *)
let gen_class cdesc = gen_vtable cdesc.method_table
(* |translate| -- generate code for the whole program *)
let translate (Program (main_mdesc,classDecls)) =
  let cdescs = List.map (fun (ClassDecl (cd,fd)) -> cd) classDecls in
  (* generate code for each class *)
  List.iter gen_class cdescs;
  (* generate code for the main method *)
  gen_method main_mdesc;
