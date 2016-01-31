(* tree.mli *)

open Env

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

val classDesc : string -> string -> class_desc
val methodDesc : string -> string -> method_desc
val variableDesc : string -> string -> variable_desc

(** |expr_desc| *)
type expr_desc = 
  { guts : expr;                            (* the actual expression *)
    mutable expression_type : string option }         (* the annotated type's class name *)

(** |expr| type representing expressible values *)
and expr = 
    Number of int
  | Variable of string
  | Monop of Keiko.op * expr_desc
  | Binop of Keiko.op * expr_desc * expr_desc
  | Call of string * string * expr_desc list

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

val exprDesc : expr -> expr_desc

type program = Program of main_decl * class_decl list

val seq : stmt list -> stmt

(* |print_tree| -- pretty-print a tree *)
val print_tree : out_channel -> program -> unit