(* ppc/keiko.ml *)

open Print

(* |symbol| -- global symbols *)
type symbol = string

type codelab = int

let nolab = -1

(* |lab| -- last used code label *)
let lab = ref 0

(* |label| -- allocate a code label *)
let label () = incr lab; !lab

(* |fLab| -- format a code label for printf *)
let fLab n = fMeta "L$" [fNum n]

let nosym = "*nosym*"

let gensym () = sprintf "g$" [fNum (label ())]

(* |op| -- type of picoPascal operators *)
type op = Plus | Minus | Times | Div | Mod | Eq 
  | Uminus | Lt | Gt | Leq | Geq | Neq | And | Or | Not | PlusA
  | Lsl | Lsr | Asr | BitAnd | BitOr | BitNot

(* |code| -- type of intermediate instructions *)
type code =
    CONST of int 		(* Constant (value) *)
  | GLOBAL of symbol		(* Constant (symbol) *)
  | LOCAL of int		(* Local address (offset) *)
  | LOAD of int			(* Load (size) *)
  | STORE of int		(* Store (size) *)
  | FIXCOPY 			(* Copy multiple values (size) *)
  | PCALL of int * int 		(* Call procedure (nparams, rsize) *)
  | RETURN of int		(* Procedure return (rsize) *)
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

  | LDL of int * int		(* LDL (n, s) = LOCAL n / LOAD s *)
  | STL of int * int		(* STL (n, s) = LOCAL n / STORE s *)
  | LDG of symbol * int		(* LDG (x, s) = GLOBAL x / LOAD s *)
  | STG of symbol * int		(* STG (x, s) = GLOBAL x / STORE s *)
  | LDNW of int			(* LDNW n = CONST n / PLUSA / LOAD 4 *)
  | STNW of int			(* STNW n = CONST n / PLUSA / STORE 4 *)
  | LDI of int			(* LDI s = CONST s / TIMES / PLUSA / LOAD s *)
  | STI of int			(* STI s = CONST s / TIMES / PLUSA / STORE s *)
  | JUMPCZ of op * codelab	(* Conditional branch with zero (cond, dest) *)

  | SEQ of code list		(* Sequence of other instructions *)
  | NOP				(* Null operation *)

let mark_line n ys =
  if n = 0 then ys else
    match ys with
	[] | LINE _ :: _ -> ys
      | _ -> LINE n :: ys

let canon x =
  let rec accum x ys =
    match x with
        SEQ xs -> List.fold_right accum xs ys
      | NOP -> ys
      | LINE n -> mark_line n ys
      | _ -> x :: ys in
  SEQ (accum x [])

let op_name =
  function
      Plus -> "PLUS" | Minus -> "MINUS" | Times -> "TIMES"
    | Div -> "DIV" | Mod -> "MOD" | Eq -> "EQ"
    | Uminus -> "UMINUS" | Lt -> "LT" | Gt -> "GT" 
    | Leq -> "LEQ" | Geq -> "GEQ" | Neq -> "NEQ" 
    | And -> "AND" | Or -> "OR" | Not -> "NOT"
    | PlusA -> "PLUSA" | Lsl -> "LSL" | Lsr -> "LSR" | Asr -> "ASR"
    | BitAnd -> "BITAND" | BitOr -> "BITOR" | BitNot -> "BITNOT"

let fOp w = fStr (op_name w)

let fType =
  function 1 -> fStr "C" | 4 -> fStr "W" | s -> fMeta "*$*" [fNum s]

let fType1 =
  function 0 -> fStr "" | 1 -> fStr "W" | s -> fMeta "*$*" [fNum s]

let fInst =
  function
      CONST x ->	fMeta "CONST $" [fNum x]
    | GLOBAL a -> 	fMeta "GLOBAL $" [fStr a]
    | LOCAL n ->	fMeta "LOCAL $" [fNum n]
    | LOAD s -> 	fMeta "LOAD$" [fType s]
    | STORE s ->     	fMeta "STORE$" [fType s]
    | FIXCOPY ->	fStr "FIXCOPY"
    | PCALL (n, s) ->	fMeta "PCALL$ $" [fType1 s; fNum n]
    | RETURN s ->	fMeta "RETURN$" [fType1 s]
    | MONOP w ->  	fMeta "$" [fStr (op_name w)]
    | BINOP w ->  	fMeta "$" [fStr (op_name w)]
    | BOUND n ->	fMeta "BOUND $" [fNum n]
    | NCHECK n ->	fMeta "NCHECK $" [fNum n]
    | ERETURN n -> 	fMeta "ERROR E_RETURN $" [fNum n]
    | LABEL l ->	fMeta "LABEL $" [fLab l]
    | JUMP l ->		fMeta "JUMP $" [fLab l]
    | JUMPC (w, l) ->   fMeta "J$ $" [fStr (op_name w); fLab l]
    | JCASE labs ->     fMeta "JCASE $" [fNum (List.length labs)]
    | LINE n ->		fMeta "LINE $" [fNum n]

    | LDL (n, s) ->	fMeta "LDL$ $" [fType s; fNum n]
    | STL (n, s) ->	fMeta "STL$ $" [fType s; fNum n]
    | LDG (x, s) ->	fMeta "LDG$ $" [fType s; fStr x]
    | STG (x, s) ->	fMeta "STG$ $" [fType s; fStr x]
    | LDNW n ->		fMeta "LDNW $" [fNum n]
    | STNW n ->		fMeta "STNW $" [fNum n]
    | LDI s ->		fMeta "LDI$" [fType s]
    | STI s ->		fMeta "STI$" [fType s]
    | JUMPCZ (w, lab) -> fMeta "J$Z $" [fStr (op_name w); fLab lab]

    | SEQ _ ->		fStr "SEQ ..."
    | NOP ->		fStr "NOP"

(* |output| -- output code sequence *)
let output code =
  let line = ref 0 in
  let rec out =
    function 
        SEQ xs -> List.iter out xs
      | NOP -> ()
      | LINE n -> 
	  if !line <> n then begin
	    printf "! $\n" [fStr (Source.get_line n)];
	    line := n
	  end
      | JCASE labs ->
          printf "$\n" [fInst (JCASE labs)];
          List.iter (fun lab -> printf "CASEL $\n" [fLab lab]) labs
      | x -> printf "$\n" [fInst x] in
  out code


let int_of_bool b = if b then 1 else 0

(* |do_monop| -- evaluate unary operators *)
let do_monop w x =
  match w with
      Uminus -> - x
    | Not -> if x <> 0 then 0 else 1
    | BitNot -> lnot x
    | _ -> failwith "do_monop"

(* |do_binop| -- evaluate binary operators *)
let do_binop w x y =
  match w with
      Plus -> x + y
    | Minus -> x - y
    | Times -> x * y
    | Div -> x / y
    | Mod -> x mod y
    | Eq -> int_of_bool (x = y)
    | Lt -> int_of_bool (x < y)
    | Gt -> int_of_bool (x > y)
    | Leq -> int_of_bool (x <= y)
    | Geq -> int_of_bool (x >= y)
    | Neq -> int_of_bool (x <> y)
    | And -> if x <> 0 then y else 0
    | Or -> if x <> 0 then 1 else y
    | PlusA -> x + y
    | Lsl -> x lsl y
    | Lsr -> x lsr y
    | Asr -> x asr y
    | BitAnd -> x land y
    | BitOr -> x lor y
    | _ -> failwith (sprintf "do_binop $" [fOp w])

(* |negate| -- negation of a comparison *)
let negate = 
  function Eq -> Neq | Neq -> Eq | Lt  -> Geq
    | Leq -> Gt | Gt  -> Leq | Geq -> Lt
    | _ -> failwith "negate"
