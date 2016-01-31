(* tree.ml *)

open Env

(** |class_desc| *)
type class_desc = 
  { name : string;                                 (* name of the class *)
    parent_name : string;                          (* name of the parent class *) 
    mutable methods_list : method_desc list;       (* list of class' method names  *)
    mutable variables_list : variable_desc list }  (* list of class' field names   *)

let classDesc n p =                         (* creates an unannotated class_desc *)
  { name = n; parent_name = p; methods_list = []; variables_list = [] }

(** |method_desc| *)
type method_desc = 
  { name : string;                          (* the method name *)
    mutable def : env_def option }          (* environmental defintion *)

let methodDesc n =                          (* creates an unannotated method_desc *)
  { name = n; env_def = None }

(** |variable_desc| *)
type variable_desc = 
  { name : string;                          (* the variables' name *)  
    mutable def : env_def option }          (* environmental definition *)

let variableDesc n =                        (* creates an unannotated variable_desc *)
  { name = n; env_def = None }

(** |expr_desc| *)
type expr_desc = 
  { guts : expr;                            (* the actual expression *)
    mutable ctype : string option }         (* the annotated type's class name *)

let exprDesc e =                            (* creates an unannotated expr_desc *)
  { guts = e; ctype = None}

(** |expr| type representing expressible values *)
and expr = 
    Number of int
  | Variable of variable_desc
  | Monop of Keiko.op * expr_desc
  | Binop of Keiko.op * expr_desc * expr_desc
  | Call of variable_desc * method_desc * expr_desc list

(** |stmt| type representing statements *)
and stmt =
    Skip
  | Seq of stmt list
  | ClassDecl of class_desc * stmt list
  | VarDecl of variable_desc * expr_desc
  | Assign of variable_desc * expr_desc
  | Return of expr_desc
  | IfStmt of expr_desc * stmt * stmt
  | WhileStmt of expr_desc * stmt
  | Print of expr_desc
  | Newline

(** |decl| type representing class, method and variable declerations *)
and decl = 
    VarDecl of variable_desc 
  | MethDecl of method_desc * stmt list
  | ClassDecl of class_desc * decl list
  | MainDecl of stmt list

type program = Program of decl list 

let seq = function
    [] -> Skip
  | [s] -> s
  | ss -> Seq ss  