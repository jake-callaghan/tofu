(* tree.ml *)

(** |class_desc| *)
type class_desc = 
  { class_name : string;                           (* name of the class *)
    parent_name : string;                          (* name of the parent class *) 
    (* pointers to method descriptors defined by this class, added during type-checking *)
    mutable methods : (method_desc ref) list;            
    (* pointers to instance variable descriptors of the class, added during type-checking *)
    mutable variables : (variable_desc ref) list;       
  }

(** |method_desc| *)
and method_desc = 
  { method_name : string;                   (* the method name *)
    mutable defining_class : class_desc;    (* pointer to class_desc of the defining class, added during type-checking *)
    return_type : string;                   (* name of class type returned *)
    number_of_formals : int;                (* the number of formal parameters required *)
    formals : formal list;                  (* the formal parameters *)
  }

(** |variable_desc| *)
and variable_desc = 
  { variable_name : string;                 (* the variables' name *)  
    variable_type : string;                 (* the variables' static type *)
  }

(** |expr_desc| *)
and expr_desc = 
  { guts : expr;                            (* the actual expression *)
  }

(** |expr| type representing expressible values *)
and expr = 
    Number of int
  | Variable of string
  | New of string
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
    InstanceVarDecl of variable_desc
  | MethDecl of method_desc * formal list * stmt 

and formal = Formal of string * string

and class_decl = ClassDecl of class_desc * feature_decl list

and main_decl = MainDecl of stmt 

let classDesc n p =                         (* creates an unannotated class_desc *)
  { class_name = n; parent_name = p; methods = []; variables = [] }
let methodDesc n rt formals1 =              (* creates an unannotated method_desc *)
  { method_name = n; defining_class = classDesc "Object" ""; return_type = rt; formals = formals1; number_of_formals = List.length formals1; }
let variableDesc n t =                      (* creates an unannotated variable_desc *)
  { variable_name = n; 
    variable_type = t;
  }
let exprDesc e =                (* creates an unannotated expr_desc *)
  { guts = e; }

type program = Program of main_decl * class_decl list

let seq = function
    [] -> Skip
  | [s] -> s
  | ss -> Seq ss  

(* Mike Spivey's Pretty printer with added tofu syntax tree constructs *)

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
    | New cname ->
        fMeta "New_$" [fStr cname]
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
        InstanceVarDecl vd ->
          fMeta "InstanceVarDecl_($, $)" [fStr vd.variable_name; fStr vd.variable_type]
      | MethDecl (md,fs,ss) ->
          fMeta "MethDecl_($, $, $, $)" [fStr md.method_name; fStr md.return_type; fList(fFormal) fs; fStmt ss]

let rec fClass (ClassDecl (cd,fs)) =
  fMeta "ClassDecl_($, $, $)" [fStr cd.class_name; fStr cd.parent_name; fList(fFeature) fs]

let rec fMain (MainDecl ss) = 
  fMeta "Main_($)" [fStmt ss]

let print_tree fp (Program (main_decl,class_decls)) =
  fgrindf fp "" "Program_($, $)" [fMain main_decl; fList(fClass) class_decls]
