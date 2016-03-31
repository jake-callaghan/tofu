(* tofu/typecheck.ml *)

open Tree
open Env

(** |line_no| -- keeps track of line number for any errors messages *)
let line_no = ref 1

(** |annotate| -- check AST for type errors and flesh out definitions *)
let annotate (Program (mainDecl,classDecls)) = ()

(** |check_stmt| ** -- check and annotate statements within method md, class cd *)
let rec check_stmt env cd md = function
	  Skip -> ()
	| Seq ss -> 
		List.iter (check_stmt env) ss
	| UnitCall (ed,mname,eds) ->
		check_method env (ed,mname,eds)
	| LocalVarDecl vd ->
		

and check_method env (e_desc,m_name,arg_e_descs) = 
	()
