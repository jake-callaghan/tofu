(* tofu/tree.ml *)
(* contains everything needed to generate the ASTs *
 * nodes are created during parsing of the (tokenized) source *
 * descriptors are to be annotated during semantic analysis *
 *)

open Keiko

(*******************************)
(* class/method/var decriptors *)
(*******************************)

(** |vtable| -- the method tables used to perform dynamic dispatch at runtime *)
type vtable = {
  mutable address : int;                   (* the assigned runtime label of this table *)
  mutable methods : method_desc list       (* the methods to be pointed to by the table at runtime *)
}

(** |class_desc| *)
and class_desc =
  { class_name : string;                           (* name of the class *)
    parent_name : string;                          (* name of the parent class *)
    mutable parent_desc : class_desc option;       (* pointer to parent class' class_desc *)
    mutable variables : variable_desc list;        (* pointers to instance variable descriptors of the class *)
    mutable method_table : vtable;                 (* holds a list of method_descs appropriate to the class *)
  }

(** |method_desc| *)
and method_desc =
  { method_name : string;                        (* the method name *)
    mutable defining_class : class_desc option;  (* pointer to class_desc of the defining class, added during type-checking *)
    return_type : string;                        (* name of class type returned *)
    number_of_formals : int;                     (* the number of formal parameters required *)
    formals : formal list;                       (* the formal parameters *)
    body : stmt;                                 (* the method's body of statements *)
    code : Keiko.code;                           (* allows library methods to be written directly in Keiko *)
    mutable vtable_index : int;                  (* the index of this method in the vtable s.t. offset := 4 * vtable_index *)
    mutable locals : variable_desc list;         (* variable descriptors of this method's locally defined vars *)
  }

(** |variable_desc| *)
and variable_desc =
  { variable_name : string;                   (* the variables' name *)
    mutable variable_type : string option;    (* the variables' static type *)
    mutable variable_kind : var_kind option;  (* the type of reference this is *)
    mutable offset : int;                     (* offset if local, arg or a field *)
  }

(** |var| **)
and var_kind =
  | Field   (* the field of a class *)
  | Local   (* a variable defined within a method *)
  | Arg     (* a variable that was passed as a parameter to a method *)

(** |expr_desc| *)
and expr_desc =
  { expr_guts : expr;                       (* the actual expression *)
    mutable expr_type : string option;      (* the type of the expression *)
  }

(***********************)
(* AST tree node types *)
(***********************)

(** |expr| type representing expressible values *)
and expr =
  | This
  | Number of int
  | Boolean of bool
  | Variable of variable_desc
  | NewObject of string
  | Call of expr_desc * string * expr_desc list

(** |stmt| type representing statements *)
and stmt =
    Skip
  | Seq of stmt list
  | UnitCall of expr_desc * string * expr_desc list
  | LocalVarDecl of variable_desc * string
  | AssignStmt of variable_desc * expr_desc
  | ReturnStmt of expr_desc
  | IfStmt of expr_desc * stmt * stmt
  | WhileStmt of expr_desc * stmt
  | PrintStmt of expr_desc
  | Newline

(** declerative types that define features of classes (vars and methods), classes and the main *)
and feature_decl =
    InstanceVarDecl of variable_desc * string
  | MethDecl of method_desc

and formal = Formal of string * string

and class_decl = ClassDecl of class_desc * feature_decl list

and main_body = MainBody of stmt

type program = Program of main_body * class_decl list

(***************************)
(* descriptor constructors *)
(***************************)

(* creates an unannotated class descriptor *)
let classDesc n p = {
  class_name = n;
  parent_name = p;
  parent_desc = None;
  variables = [];
  method_table = { address = -1; methods = []; }
};;

(* creates an unannotated method decriptor *)
let methodDesc n rt formals1 body1 = {
  method_name = n;
  defining_class = None;
  return_type = rt;
  number_of_formals = List.length formals1;
  formals = formals1;
  body = body1;
  code = Keiko.NOP;
  vtable_index = -1; (* -1 indicates that this has yet to be assigned *)
  locals = [];
};;

(* creates an unannotated variable descriptor *)
let variableDesc n = {
  variable_name = n;
  variable_type = None;
  variable_kind = None;
  offset = -1;
};;

(* creates an unannotated expression descriptor *)
let exprDesc e = {
  expr_guts = e;
  expr_type = None;
};;

let seq = function
    [] -> Skip
  | [s] -> s
  | ss -> Seq ss;;

(* Mike Spivey's Pretty printer with added tofu syntax tree constructs *)
open Print

let fTail f xs =
  let g prf = List.iter (fun x -> prf "; $" [f x]) xs in fExt g

let fList f =
  function
      [] -> fStr "[]"
    | x::xs -> fMeta "[$$]" [f x; fTail(f) xs]

let fInt i = fStr (string_of_int i)

let fStrOpt o = match o with
| Some s -> fStr ("="^s)
| None   -> fStr "=None"

let fBool o = match o with
| true -> fStr "true"
| false -> fStr "false"

let fKindOpt o =
  let str = match o with
  | Some Field  -> "Field"
  | Some Local  -> "Local"
  | Some Arg    -> "Arg"
  | _      -> "None"
  in fStr ("="^str)

let fClassDescOpt o = match o with
| Some cd -> fStr ("="^cd.class_name)
| None    -> fStr "=None"

let fVarDesc vd =
  fMeta "Variable($,$,$,$)" [fStr vd.variable_name; fStrOpt vd.variable_type; fKindOpt vd.variable_kind; fInt vd.offset]

let rec fExprDesc ed = match ed.expr_guts with
    | This ->
        fMeta "This_($)" [fStrOpt ed.expr_type]
    | Number n ->
        fMeta "Number_($,$)" [fNum n; fStrOpt ed.expr_type]
    | Boolean b ->
        fMeta "Boolean_($,$)" [fBool b; fStrOpt ed.expr_type]
    | Variable vd ->
        fVarDesc vd
    | NewObject cname ->
        fMeta "New_($,$)" [fStr cname; fStrOpt ed.expr_type]
    | Call (ed, meth, eds) ->
        fMeta "Call_($, $, $, $)" [fExprDesc ed; fStr meth; fList(fExprDesc) eds; fStrOpt ed.expr_type]

let rec fStmt =
  function
      Skip ->
        fStr "Skip"
    | Seq ss ->
        fMeta "Seq_$" [fList(fStmt) ss]
    | UnitCall (ed, meth, eds) ->
        fMeta "Call_($, $, $)" [fExprDesc ed; fStr meth; fList(fExprDesc) eds]
    | LocalVarDecl (vd,t) ->
        fMeta "LocalVarDecl_($, $, $)" [fStr vd.variable_name; fStr t; fStrOpt vd.variable_type]
    | AssignStmt (vd, ed) ->
        fMeta "Assign_($, $)" [fVarDesc vd; fExprDesc ed]
    | ReturnStmt ed ->
        fMeta "Return_($)" [fExprDesc ed]
    | PrintStmt ed ->
        fMeta "Print_($)" [fExprDesc ed]
    | Newline ->
        fStr "Newline"
    | IfStmt (ed, s1, s2) ->
        fMeta "IfStmt_($, $, $)" [fExprDesc ed; fStmt s1; fStmt s2]
    | WhileStmt (ed, s) ->
        fMeta "WhileStmt_($, $)" [fExprDesc ed; fStmt s]

let rec fFormal (Formal (n,t)) =
  fMeta "Formal_($, $)" [fStr n; fStr t]

let fMethodDesc md =
  fMeta "MethDecl_($,$,$,$,$,$,$,$)" [
    fStr md.method_name;
    fStr md.return_type;
    fClassDescOpt md.defining_class;
    fInt md.number_of_formals;
    fList(fFormal) md.formals;
    fInt md.vtable_index;
    fList(fVarDesc) md.locals;
    fStmt md.body; ]

let fVTable vt =
  fMeta "VTable_($,$)" [fInt vt.address; fList(fMethodDesc) vt.methods]

let rec fClass (ClassDecl (cd,fs)) =
  fMeta "ClassDecl_($, $, $, $, $)" [
    fStr cd.class_name;
    fStr cd.parent_name;
    fClassDescOpt cd.parent_desc;
    fList(fVarDesc) cd.variables;
    fVTable cd.method_table ]

let rec fMain (MainBody ss) =
  fMeta "Main_($)" [fStmt ss]

let print_tree fp (Program (main_decl,class_decls)) =
  fgrindf fp "" "Program_($, $)" [fMain main_decl; fList(fClass) class_decls]
