(* tofu/tree.mli *)
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
    mutable code : Keiko.code;                   (* keiko code *)
    mutable vtable_index : int;                  (* the index of this method in the vtable s.t. offset := 4 * vtable_index *)
    mutable locals : variable_desc list;         (* variable descriptors of this method's locally defined vars *)
  }

(** |variable_desc| *)
and variable_desc =
  { variable_name : string;                   (* the variables' name *)
    mutable variable_type : string option;    (* the variables' static type *)
    mutable variable_kind : var_kind option;  (* the type of reference this is *)
    mutable offset : int;                     (* offset w.r.t local, arg or a class field *)
  }

(** |var| **)
and var_kind =
  | Field   (* the field of a class *)
  | Local   (* a variable defined within a method *)
  | Arg     (* a variable that was passed as a parameter to a method *)

(** |expr_desc| *)
and expr_desc =
  { expr_guts : expr;                       (* the actual expression *)
    mutable expr_type : string option;      (* the annotated type of the expression *)
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
  | Newline

(** declerative types that define features of classes (vars and methods), classes and the main *)

and feature_decl =
    InstanceVarDecl of variable_desc * string
  | MethDecl of method_desc

(** param_name x param_type *)
and formal = Formal of string * string

and class_decl = ClassDecl of class_desc * feature_decl list

type program = Program of method_desc * class_decl list

(***************************)
(* descriptor constructors *)
(***************************)

val classDesc : string -> string -> class_desc
val methodDesc : string -> string -> formal list -> stmt -> method_desc
val variableDesc : string -> variable_desc
val exprDesc : expr -> expr_desc
val print_vdesc : variable_desc -> unit
val seq : stmt list -> stmt

(* |print_tree| -- pretty-print a tree *)
val print_tree : out_channel -> program -> unit
