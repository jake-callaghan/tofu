(* tree.mli *)

(** |env_def| represents the annotated AST information *)
type env_def = { 
  class_name : string;      (* dynamic type *)
            
}

(** |class_desc| *)
type class_desc = 
  { class_name : string;                           (* name of the class *)
    parent_name : string;                          (* name of the parent class *) 
    (* mutable vtable TO DO *)
  }

(** |method_desc| *)
and method_desc = 
  { method_name : string;                   (* the method name *)
    return_type : string;                   (* name of class type returned *)
    mutable number_of_formals : int;        (* the number of formal parameters required *)
    formals : formal list;                  (* the formal parameters *)
    mutable method_def : env_def option }   (* environmental defintion *)

(** |variable_desc| *)
and variable_desc = 
  { variable_name : string;                 (* the variables' name *)  
    variable_type : string;                 (* the variables' static type *)
    mutable variable_def : env_def option } (* environmental definition *)

(** |expr_desc| *)
and expr_desc = 
  { guts : expr;                                      (* the actual expression *)
    mutable expression_type : string option }         (* the type's class name *)

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

(** param_name x param_type *)
and formal = Formal of string * string

and class_decl = ClassDecl of class_desc * feature_decl list

and main_decl = MainDecl of stmt

val exprDesc : expr -> expr_desc
val classDesc : string -> string -> class_desc
val methodDesc : string -> string -> formal list -> method_desc
val variableDesc : string -> string -> variable_desc

type program = Program of main_decl * class_decl list

val seq : stmt list -> stmt

(* |print_tree| -- pretty-print a tree *)
val print_tree : out_channel -> program -> unit