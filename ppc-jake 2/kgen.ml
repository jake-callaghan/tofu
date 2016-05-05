(* ppc/kgen.ml *)

open Dict
open Tree
open Mach
open Keiko
open Lexer
open Print

(* This code generator is a bit more like a functional program,
   because each generation routine returns a list of instructions,
   represented using the SEQ constructor of Keiko.inst *)

let optflag = ref false
let boundchk = ref false

(* |level| -- nesting level of current procedure *)
let level = ref 0

(* |size_of| -- calculate size of type *)
let size_of t = t.t_rep.r_size

(* |count_of| -- calculate number of parameter words *)
let count_of t = if t.t_rep.r_size = 0 then 0 else 1

(* |is_const| -- test if expression is a constant *)
let is_const e = (e.e_value <> None)

(* |get_value| -- get constant value or fail *)
let get_value e =
  match e.e_value with
      Some v -> v
    | None -> failwith "get_value"

(* |arg_size| -- compute size of argument *)
let arg_size f =
  match f.d_kind with PParamDef -> 2 | _ -> 1

(* |line_number| -- compute line number of variable for bound check *)
let rec line_number v =
  match v.e_guts with
      Variable x -> x.x_line
    | Sub (a, i) -> line_number a
    | Select (r, x) -> x.x_line
    | Deref p -> line_number p
    | _ -> failwith "line_number"

let load_addr = LOAD addr_rep.r_size

(* |schain| -- code to follow N links of static chain *)
let schain n =
  if n = 0 then
    LOCAL 0
  else 
    SEQ [LOCAL stat_link; load_addr;
      SEQ (Util.copy (n-1) (SEQ [CONST stat_link; BINOP PlusA; load_addr]))]

(* |address| -- code to push address of an object *)
let address d =
  match d.d_addr with
      Global g -> GLOBAL g
    | Local off -> 
	if d.d_level = !level then 
	  LOCAL off
	else
	  SEQ [schain (!level - d.d_level); CONST off; BINOP PlusA]
    | _ -> 
	failwith (sprintf "address $" [fId d.d_tag])

(* |gen_closure| -- push a (code, envt) pair *)
let gen_closure d =
  match d.d_kind with
      ProcDef ->
        let statlink =
	  if d.d_level = 0 then CONST 0
	  else schain (!level - d.d_level) in
        SEQ [statlink; address d]
    | PParamDef ->
	SEQ [address d; CONST addr_rep.r_size; BINOP PlusA; load_addr;
	  address d; load_addr]
    | _ -> failwith "missing closure"

(* |gen_addr| -- code for the address of a variable *)
let rec gen_addr v = 
  match v.e_guts with
      Variable x ->
	let d = get_def x in
	begin
	  match d.d_kind with
	      VarDef ->
		address d
	    | VParamDef ->
		SEQ [address d; load_addr]
	    | CParamDef ->
		if scalar d.d_type || is_pointer d.d_type then 
		  address d
		else
		  SEQ [address d; load_addr]
	    | StringDef ->
		address d
	    | _ -> 
		failwith "load_addr"
	end
    | Sub (a, i) ->
	SEQ [gen_addr a; gen_expr i;
	  if !boundchk then
	    SEQ [CONST (bound a.e_type); BOUND (line_number a)]
	  else
	    NOP;
	  CONST (size_of v.e_type); BINOP Times; BINOP PlusA]
    | Select (r, x) ->
        let d = get_def x in
	SEQ [gen_addr r; CONST (offset_of d); BINOP PlusA]
    | Deref p ->
	SEQ [gen_expr p;
	  if !boundchk then NCHECK (line_number p) else NOP]
    | String (lab, n) ->
	GLOBAL lab
    | _ -> failwith "gen_addr"

(* |gen_expr| -- tree for the value of an expression *)
and gen_expr e =
  match e.e_value with
      Some v -> 
	CONST v
    | None -> 
	begin
	  match e.e_guts with
	      Variable _ | Sub _ | Select _ | Deref _ ->
		SEQ [gen_addr e; LOAD (size_of e.e_type)]
	    | (Monop (Not, _) | Binop ((And|Or), _, _) ) ->
		gen_cond_expr e
	    | Monop (w, e1) ->
		SEQ [gen_expr e1; MONOP w]
	    | Binop (w, e1, e2) ->
		SEQ [gen_expr e1; gen_expr e2; BINOP w]
	    | FuncCall (p, args) -> 
		gen_call p args
	    | _ -> failwith "gen_expr"
	end

(* |gen_call| -- generate code to call a procedure *)
and gen_call x args =
  let d = get_def x in
  match d.d_kind with
      LibDef q ->
	gen_libcall q args d.d_type
    | _ ->
	let p = get_proc d.d_type in
	SEQ [
	  SEQ (List.map gen_arg (List.rev (List.combine p.p_fparams args)));
	  gen_closure d;
	  PCALL (p.p_pcount, count_of p.p_result)]

(* |gen_arg| -- generate code to push a procedure argument *)
and gen_arg (f, a) = 
  match f.d_kind with
      CParamDef ->
	if scalar f.d_type || is_pointer f.d_type then 
	  gen_expr a
	else 
	  gen_addr a
    | VParamDef ->
	gen_addr a
    | PParamDef ->
        begin
	  match a.e_guts with 
              Variable x -> 
		gen_closure (get_def x)
            | _ -> 
		failwith "bad funarg"
	end
    | _ -> failwith "bad arg"

(* |gen_libcall| -- generate code to call a built-in procedure *)
and gen_libcall q args rtype =
  let libcall lab n =
    SEQ [CONST 0; GLOBAL lab; PCALL (n, count_of rtype)] in
  match (q.q_id, args) with
      ((ChrFun|OrdFun), [e]) ->
	gen_expr e
    | (PrintString, [e]) ->
	SEQ [CONST (bound e.e_type); gen_addr e; 
	  libcall "_print_string" 2]
    | (ReadChar, [e]) ->
	SEQ [gen_addr e; libcall "_read_char" 1]
    | (NewProc, [e]) ->
	let size = size_of (base_type e.e_type) in
	SEQ [CONST size; gen_addr e; libcall "_new" 2]
    | (ArgvProc, [e1; e2]) ->
        SEQ [gen_addr e2; gen_expr e1; libcall "_argv" 2]
    | (OpenIn, [e]) ->
	SEQ [gen_addr e; libcall "_open_in" 1]
    | (Operator op, [e1]) ->
        SEQ [gen_expr e1; MONOP op]
    | (Operator op, [e1; e2]) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP op]
    | (_, _) ->
	let proc = sprintf "_$" [fLibId q.q_id] in
	SEQ [SEQ (List.map gen_expr (List.rev args));
	  libcall proc (List.length args)]

(* |gen_cond| -- generate code to branch on a condition *)
and gen_cond tlab flab test =
  match test.e_value with
      Some v ->
	if v <> 0 then JUMP tlab else JUMP flab
    | None ->
	begin
	  match test.e_guts with
	      Monop (Not, e) ->
		gen_cond flab tlab e
	    | Binop (And, e1, e2) ->
                let lab1 = label () in
                SEQ [gen_cond lab1 flab e1; LABEL lab1; gen_cond tlab flab e2]
            | Binop (Or, e1, e2) ->
                let lab1 = label () in
                SEQ [gen_cond tlab lab1 e1; LABEL lab1; gen_cond tlab flab e2]
	    | Binop ((Eq | Neq | Lt | Leq | Gt | Geq) as w, e1, e2) ->
		SEQ [gen_expr e1; gen_expr e2; JUMPC (w, tlab); JUMP flab]
	    | _ ->
		SEQ [gen_expr test; CONST 0; JUMPC (Neq, tlab); JUMP flab]
	end

(* |gen_cond_expr| -- generate short-cicuit expression with boolean result *)
and gen_cond_expr test =
  let l1 = label () and l2 = label () and l3 = label () in
  SEQ [gen_cond l1 l2 test;
    LABEL l1; CONST 1; JUMP l3; LABEL l2; CONST 0; LABEL l3]

let gen_jtable tab0 deflab =
  if tab0 = [] then JUMP deflab else begin
    let table = List.sort (fun (v1, l1) (v2, l2) -> compare v1 v2) tab0 in
    let lob = fst (List.hd table) in
    let rec tab u qs =
      match qs with
          [] -> []
        | (v, l) :: rs -> 
            if u = v then l :: tab (v+1) rs else deflab :: tab (u+1) qs in
    SEQ [CONST lob; BINOP Minus; JCASE (tab lob table); JUMP deflab]
  end

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt s = 
  let code =
    match s.s_guts with
	Skip -> NOP
      | Seq ss -> SEQ (List.map gen_stmt ss)
      | Assign (v, e) ->
	  if scalar v.e_type || is_pointer v.e_type then
	    SEQ [gen_expr e; gen_addr v; STORE (size_of v.e_type)]
	  else
	    SEQ [gen_addr v; gen_addr e;
	      CONST (size_of v.e_type); FIXCOPY]
      | ProcCall (p, args) ->
	  gen_call p args
      | Return res ->
	  begin
	    match res with
		Some e -> SEQ [gen_expr e; RETURN 1]
	      | None -> SEQ [RETURN 0]
	  end
      | IfStmt (test, thenpt, elsept) ->
          let lab1 = label () and lab2 = label () and lab3 = label () in
          SEQ [gen_cond lab1 lab2 test; 
            LABEL lab1; gen_stmt thenpt; JUMP lab3;
            LABEL lab2; gen_stmt elsept; LABEL lab3]
      | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
          SEQ [JUMP lab2; LABEL lab1; gen_stmt body; 
            LABEL lab2; gen_cond lab1 lab3 test; LABEL lab3]
      | RepeatStmt (body, test) ->
          let lab1 = label () and lab2 = label () in
          SEQ [LABEL lab1; gen_stmt body;
            gen_cond lab2 lab1 test; LABEL lab2]
      | ForStmt (var, lo, hi, body) ->
          (* For simplicity, this code re-evaluates hi on each iteration *)
          let l1 = label () and l2 = label () in
          let s = int_rep.r_size in
          SEQ [gen_expr lo; gen_addr var; STORE s; JUMP l2;
            LABEL l1; gen_stmt body;
            gen_expr var; CONST 1; BINOP Plus; gen_addr var; STORE s;
            LABEL l2; gen_expr var; gen_expr hi; JUMPC (Leq, l1)] 
      | CaseStmt (sel, arms, deflt) ->
          let deflab = label () and donelab = label () in
          let labs = List.map (function x -> label ()) arms in
          let get_val (v, body) = get_value v in
          let table = List.combine (List.map get_val arms) labs in
          let gen_case lab (v, body) =
            SEQ [LABEL lab; gen_stmt body; JUMP donelab] in
          SEQ [gen_expr sel; gen_jtable table deflab;
            SEQ (List.map2 gen_case labs arms);
            LABEL deflab; gen_stmt deflt;
            LABEL donelab] in
  SEQ [if s.s_line <> 0 then LINE s.s_line else NOP; code]

(* |do_proc| -- generate code for a procedure *)
let do_proc lab lev rtype fsize body =
  printf "PROC $ $ 0 0\n" [fStr lab; fNum fsize];
  level := lev+1;
  let code = 
    SEQ [gen_stmt body;
      (if rtype.t_rep.r_size = 0 then RETURN 0 else ERETURN 0)] in
  Keiko.output (if !optflag then Peepopt.optimise code else code);
  printf "END\n\n" []

(* |gen_proc| -- translate a procedure, ignore other declarations *)
let rec gen_proc = 
  function
      ProcDecl (Heading (x, _, _), Block (locals, body, fsize, nregv)) ->
	let d = get_def x in
	let p = get_proc d.d_type in
	begin
	  match d.d_addr with 
	      Global lab ->
		do_proc lab d.d_level p.p_result !fsize body;
		gen_procs locals
	    | _ -> failwith "gen_proc"
	end
    | _ -> ()

(* |gen_procs| -- generate code for the procedures in a block *)
and gen_procs ds = List.iter gen_proc ds

(* |gen_string| -- generate code for a string constant *)
let gen_string (lab, s) = 
  let s' = s ^ "\000" in
  printf "! String \"$\"\n" [fStr (String.escaped s)];
  printf "DEFINE $\n" [fStr lab];
  let hex = "0123456789ABCDEF" in
  let n = String.length s' and r = ref 0 in
  while !r < n do
    let k = min (n - !r) 32 in
    printf "STRING " [];
    for i = !r to !r+k-1 do
      let c = int_of_char s'.[i] in
      printf "$$" [fChr (hex.[c / 16]); fChr (hex.[c mod 16])]
    done;
    printf "\n" [];
    r := !r + k
  done;
  printf "\n" []

(* |gen_global| -- reserve space for global variables *)
let gen_global d =
  match d.d_kind with
      VarDef ->
	(match d.d_addr with
	    Global lab -> 
	      printf "GLOVAR $ $\n" [fStr lab; fNum (size_of d.d_type)]
	  | _ -> failwith "gen_global")
    | _ -> ()

(* |translate| -- generate code for the whole program *)
let translate (Prog (Block (globals, main, _, _), glodefs)) =
  gen_procs globals;
  do_proc "MAIN" 0 voidtype 0 main;
  List.iter gen_global !glodefs;
  List.iter gen_string (string_table ())
