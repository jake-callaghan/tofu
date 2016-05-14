(** tofu/kgen.ml *)
(* this module generates code for various constructs in the ASTs *)

open Errors
open Env
open Lib
open Tree
open Keiko
open Hashtbl

(* push the address of this object *)
let push_this = SEQ [LOCAL 16; LOADW]

(** |gen_var_addr| -- generate code to push a variable's address onto stack *)
let gen_var_addr mdesc vdesc =
	match unwrap vdesc.variable_kind with
  | Field -> SEQ [push_this; CONST vdesc.offset; BINOP PlusA; LOADW]
  | Local | Arg -> SEQ [LOCAL vdesc.offset; LOADW]
	| Global -> let x = ("_"^vdesc.variable_name) in GLOBAL x

(** |gen_method_call| -- generate code for "edesc2.mname(arg_edescs...)" where rw = true if a word is returned *)
let rec gen_method_call mdesc edesc2 mname arg_edescs rw =
  let argsize = (List.length arg_edescs)+1 in
  let callOp = if rw then PCALLW argsize else PCALL argsize in
  let gen_args = SEQ (List.rev_map (gen_expr mdesc) arg_edescs) in
  (*Keiko.output (Keiko.canon gen_args);*)
  let cdesc = find_class (unwrap edesc2.expr_type) in
  let mdesc2 = find_method cdesc mname in (* the descriptor of the method being called *)
  SEQ [
    gen_args;
    (*---calculate method's address---*)
    gen_expr mdesc edesc2;          (* var address *)
    DUP;                            (* 2x var address: 1 for lookup, 1 passed as argument *)
    LOADW;                          (* push vtable address *)
    CONST (4*mdesc2.vtable_index);  (* offset in vtable *)
    BINOP PlusA; LOADW;             (* push method address *)
    (*--------------------------------*)
		CONST 0;												(* static link *)
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
let gen_method cdesc mdesc =
	let cname = cdesc.class_name in
  let def_cname = (unwrap mdesc.defining_class).class_name in
  let mname = mdesc.method_name in
	(* this class defines the method -> generate code, otherwise it's just 'inherited' *)
	if def_cname = cname then mdesc.code <- Keiko.canon (gen_stmt mdesc mdesc.body) else ()

(** |gen_main_method| -- generate code for main method's body and declerations of its variables *)
let gen_main_method main_desc =
	(* code for the body *)
	main_desc.mdesc.code <- Keiko.canon (gen_stmt main_desc.mdesc main_desc.mdesc.body);
	(* code for the variables declared within body *)
	let glovars = SEQ (List.map (fun vdesc -> GLOVAR (("_"^vdesc.variable_name),4)) main_desc.mdesc.locals) in
	main_desc.decls <- glovars

(** |gen_vtable| -- gen code for all methods in the vtable *)
let gen_vtable cdesc vt = List.iter (gen_method cdesc) vt.methods

(** |gen_class| -- gen code for a class *)
let gen_class cdesc = gen_vtable cdesc cdesc.method_table

(* |translate| -- generate code for the whole program *)
let translate (Program (main_desc,classDecls)) =

	(* --- link library class descriptors to their method code --- *)
  let link_lib libcdesc libmcodes =
      List.iter (fun mdesc ->
        let (libmethname,libmcode) =
          try List.find (fun (mname,mcode) -> mname = mdesc.method_name) libmcodes
          with Not_found -> compilationError ("library method "^libcdesc.class_name^"."^mdesc.method_name^" not found.")
        in mdesc.code <- (libmcode ())
      ) libcdesc.method_table.methods in
  (* objects *)
  link_lib Object.object_desc Object_methods.object_methods_code;
  (* integers *)
  link_lib Integer.integer_desc Integer_methods.integer_methods_code;
  (* booleans *)
  link_lib Boolean.boolean_desc Boolean_methods.boolean_methods_code;

	(* --- generate code for each user-defined class --- *)
	let cdescs = List.map (fun (ClassDecl (cd,fd)) -> cd) classDecls in
  List.iter gen_class cdescs;

	(* generate code for the main method *)
  gen_main_method main_desc
