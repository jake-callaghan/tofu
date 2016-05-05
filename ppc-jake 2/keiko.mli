(* ppc/keiko.mli *)

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq 
  | And | Or | Not | PlusA | Lsl | Lsr | Asr
  | BitAnd | BitOr | BitNot

val fOp : op -> Print.arg

(* |symbol| -- global symbols *)
type symbol = string

val nosym : symbol

val gensym : unit -> symbol

(* |codelab| -- type of code labels *)
type codelab

val nolab : codelab

(* |label| -- generate a code label *)
val label : unit -> codelab

val fLab : codelab -> Print.arg

(* |code| -- type of intermediate instructions *)
type code =
    CONST of int 		(* Constant (value) *)
  | GLOBAL of symbol		(* Constant (symbol) *)
  | LOCAL of int		(* Local address (offset) *)
  | LOAD of int			(* Load (size) *)
  | STORE of int		(* Store (size) *)
  | FIXCOPY 			(* Copy multiple values (size) *)
  | PCALL of int * int 		(* Call procedure (nparams, rsize) *)
  | RETURN of int		(* Procedure result (rsize) *)
  | MONOP of op			(* Perform unary operation (op) *)
  | BINOP of op			(* Perform binary operation (op) *)
  | BOUND of int	  	(* Array bound check (line) *)
  | NCHECK of int		(* Null pointer check (line) *)
  | ERETURN of int		(* Failure to return (line) *)
  | LABEL of codelab		(* Set code label *)
  | JUMP of codelab		(* Unconditional branch (dest) *)
  | JUMPC of op * codelab	(* Conditional branch (cond, dest) *)
  | JCASE of codelab list       (* Jump table *)
  | LINE of int			(* Line number *)

  (* Compressed instructions *)
  | LDL of int * int		(* LDL (n, s) = LOCAL n / LOAD s *)
  | STL of int * int		(* STL (n, s) = LOCAL n / STORE s *)
  | LDG of symbol * int		(* LDG (x, s) = GLOBAL x / LOAD s *)
  | STG of symbol * int		(* STG (x, s) = GLOBAL x / STORE s *)
  | LDNW of int 		(* LDNW n = CONST n / PLUSA / LOAD 4 *)
  | STNW of int 		(* STNW n = CONST n / PLUSA / STORE 4 *)
  | LDI of int			(* LDI s = CONST s / TIMES / PLUSA / LOAD s *)
  | STI of int			(* STI s = CONST s / TIMES / PLUSA / STORE s *)
  | JUMPCZ of op * codelab	(* JUMPCZ (w, a) = CONST 0 / JUMPC (w, a) *)

  | SEQ of code list		(* Sequence of other instructions *)
  | NOP				(* Null operation *)

(* canon -- eliminate SEQ and NOP nodes and unneeded LINE's *)
val canon : code -> code

(* |fInst| -- printf format for instructions *)
val fInst : code -> Print.arg

(* |output| -- output code sequence *)
val output : code -> unit

(* |do_monop| -- evaluate unary operation *)
val do_monop : op -> int -> int

(* |do_binop| -- evaluate binary operation *)
val do_binop : op -> int -> int -> int

(* |negate| -- find opposite for comparison op *)
val negate : op -> op
