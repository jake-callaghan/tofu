(* tree.ml *)

(** |env_def| represents the annotated AST information *)
type env_def = { 
  class_name : string;      (* dynamic class type *)
            
}

(** |class_desc| *)
type class_desc = 
  { class_name : string;                           (* name of the class *)
    parent_name : string;                          (* name of the parent class *) 
    mutable methods_list : method_desc list;       (* list of class' method names  *)
    mutable variables_list : variable_desc list }  (* list of class' field names   *)

(** |method_desc| *)
and method_desc = 
  { method_name : string;                   (* the method name *)
    return_type : string;                   (* name of class type returned *)
    mutable method_def : env_def option }   (* environmental defintion *)

(** |variable_desc| *)
and variable_desc = 
  { variable_name : string;                 (* the variables' name *)  
    variable_type : string;                 (* the variables' static type *)
    mutable variable_def : env_def option } (* environmental definition *)

let classDesc n p =                         (* creates an unannotated class_desc *)
  { class_name = n; parent_name = p; methods_list = []; variables_list = [] }
let methodDesc n rt =                       (* creates an unannotated method_desc *)
  { method_name = n; return_type = rt; method_def = None }
let variableDesc n t =                      (* creates an unannotated variable_desc *)
  { variable_name = n; 
    variable_type = t;
    variable_def = None }

(** |expr_desc| *)
type expr_desc = 
  { guts : expr;                                (* the actual expression *)
    mutable expression_type : string option }   (* the annotated type's class name *)

(** |expr| type representing expressible values *)
and expr = 
    Number of int
  | Variable of string
  | Call of expr_desc * string * expr_desc list

(** |stmt| type representing statements *)
and stmt =
    Skip
  | Seq of stmt list
  | LocalVarDecl of variable_desc
  | AssignStmt of string * expr_desc
  | ReturnStmt of expr_desc
  | IfStmt of expr_desc * stmt * stmt
  | WhileStmt of expr_desc * stmt
  | PrintStmt of expr_desc
  | Newline

(** declerative types that define features of classes (vars and methods), classes and the main *)
and feature_decl = 
    ClassVarDecl of variable_desc
  | MethDecl of method_desc * formal list * stmt 

and formal = Formal of string * string

and class_decl = ClassDecl of class_desc * feature_decl list

and main_decl = MainDecl of stmt 

type program = Program of main_decl * class_decl list

let exprDesc e =                (* creates an unannotated expr_desc *)
  { guts = e; expression_type = None}

let seq = function
    [] -> Skip
  | [s] -> s
  | ss -> Seq ss  

(* Mike Spivey's Pretty printer with tofu AS constructs *)

open Print

let fTail f xs =
  let g prf = List.iter (fun x -> prf "; $" [f x]) xs in fExt g

let fList f =
  function
      [] -> fStr "[]"
    | x::xs -> fMeta "[$$]" [f x; fTail(f) xs]

let fName x = fStr x.variable_name

let gutter ed = ed.guts

let rec fExpr =
  function
      Number n ->
        fMeta "Number_$" [fNum n]
    | Variable x -> 
        fMeta "Variable_$" [fStr x]
    | Call (ed, meth, es) ->
        fMeta "Call_($, $, $)" [fExpr (gutter ed); fStr meth; fList(fExpr) (List.map gutter es)]

let rec fStmt = 
  function
      Skip -> 
        fStr "Skip"
    | Seq ss -> 
        fMeta "Seq_$" [fList(fStmt) ss]
    | LocalVarDecl vd ->
        fMeta "LocalVarDecl_($, $)" [fStr vd.variable_name; fStr vd.variable_type]
    | AssignStmt (x, e) -> 
        fMeta "Assign_($, $)" [fStr x; fExpr e.guts]
    | ReturnStmt e ->
        fMeta "Return_($)" [fExpr e.guts]
    | PrintStmt e -> 
        fMeta "Print_($)" [fExpr e.guts]
    | Newline -> 
        fStr "Newline"
    | IfStmt (e, s1, s2) ->
        fMeta "IfStmt_($, $, $)" [fExpr e.guts; fStmt s1; fStmt s2]
    | WhileStmt (e, s) -> 
        fMeta "WhileStmt_($, $)" [fExpr e.guts; fStmt s]

let rec fFormal (Formal (n,t)) = 
  fMeta "Formal_($, $)" [fStr n; fStr t]

let rec fFeature = 
    function
        ClassVarDecl vd ->
          fMeta "ClassVarDecl_($, $)" [fStr vd.variable_name; fStr vd.variable_type]
      | MethDecl (md,fs,ss) ->
          fMeta "MethDecl_($, $, $, $)" [fStr md.method_name; fStr md.return_type; fList(fFormal) fs; fStmt ss]

let rec fClass (ClassDecl (cd,fs)) =
  fMeta "ClassDecl_($, $, $)" [fStr cd.class_name; fStr cd.parent_name; fList(fFeature) fs]

let rec fMain (MainDecl ss) = 
  fMeta "Main_($)" [fStmt ss]

let print_tree fp (Program (main_decl,class_decls)) =
  fgrindf fp "" "Program_($, $)" [fMain main_decl; fList(fClass) class_decls]
