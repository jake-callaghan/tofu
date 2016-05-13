(* tofu/typecheck.ml *)

open Tree
open Env
open Lib
open Hashtbl
open Errors

let verbose = ref false;;

(*
let checked_methods = ref (Hashtbl.create 50);;
let checked_variables = ref (Hashtbl.create 50);;

let method_already_checked cname mname =
	try let () = Hashtbl.find !checked_methods (cname,mname) in true
	with Not_found -> false;;

let method_was_checked cname mname =
	if (!verbose) then print_string ("["^cname^"."^mname^"]\nOK\n");
	Hashtbl.add !checked_methods (cname,mname) ();;

let variable_already_checked cname vname =
	try let () = Hashtbl.find !checked_variables (cname,vname) in true
	with Not_found -> false;;

let variable_was_checked cname vname =
	if (!verbose) then print_string ("["^cname^"."^vname^"]\nOK\n");
	Hashtbl.add !checked_variables (cname,vname) ();; *)

(* check and annotate an expression descriptor found in a particular mdesc.body *)
let rec check_expr mdesc edesc =
  match edesc.expr_guts with
	  This -> let cname = (unwrap mdesc.defining_class).class_name in edesc.expr_type <- Some cname
  | Number n -> edesc.expr_type <- Some "Integer"
	| Boolean b -> edesc.expr_type <- Some "Boolean"
  | Variable vdesc ->
  		begin
  		(* is this a (declared earlier) local var? *)
  		try let vd = List.find (fun localvd -> localvd.variable_name = vdesc.variable_name) mdesc.locals in
  			vdesc.variable_kind <- Some Local;
  			vdesc.variable_type <- vd.variable_type;
  			edesc.expr_type <- vd.variable_type;
  		with Not_found ->
  			(* is this a method parameter? *)
  			let ftyper (Formal (fname,ftype)) = ftype in
  			try let fm = List.find (fun (Formal (fname,ftype)) -> fname = vdesc.variable_name) mdesc.formals in
  				vdesc.variable_kind <- Some Arg;
  				vdesc.variable_type <- Some (ftyper fm);
  				edesc.expr_type <- Some (ftyper fm);
	  		with Not_found ->
	  		(* is this a field variable of the class? *)
	  			let vd = find_instance_var (unwrap mdesc.defining_class) vdesc.variable_name in
	  			vdesc.variable_kind <- Some Field;
	  			vdesc.variable_type <- vd.variable_type;
	  			edesc.expr_type <- vdesc.variable_type;
	  	end
  | NewObject cname ->
  		(* is this a defined class? *)
  		let cdesc = find_class cname in edesc.expr_type <- Some cname
  | Call (edesc2, mname, arg_edescs) ->
  		(* get the type etype of the receiver *)
 		if edesc2.expr_type = None then check_expr mdesc edesc2 else ();
  	let etype = unwrap edesc2.expr_type in
  	(* is the method defined in class etype? *)
  	let mdesc2 = find_method (find_class etype) mname in
  	(* do the arguments obey the method's signature? *)
  	if (List.length arg_edescs <> mdesc2.number_of_formals) then argumentError ("Wrong number of arguments for method "^(unwrap mdesc2.defining_class).class_name^"."^mdesc2.method_name)
		else (* are these argument expressions subtypes of the expected types? *)
			let get_arg_type arg_edesc =
				if arg_edesc.expr_type = None then check_expr mdesc arg_edesc else () in
			let arg_types =
				List.map (get_expr_type mdesc) arg_edescs in
			let f argtype (Formal (fname,ftype)) =
				if not (is_subclass argtype ftype) then argumentError ("The expression supplied as argument "^fname^" is not of type "^ftype) else () in
				List.iter2 f arg_types mdesc2.formals;
				edesc.expr_type <- Some mdesc2.return_type
and
(* return the type name of an expression occuring in mdesc *)
get_expr_type mdesc edesc = match edesc.expr_type with
	| None -> check_expr mdesc edesc; get_expr_type mdesc edesc
	| Some cname -> cname;;

(* check the statements in a method descriptor *)
let rec check_stmt mdesc body =
	let methstr = if (mdesc.defining_class = None) then "MAIN" else (unwrap mdesc.defining_class).class_name^"."^mdesc.method_name in
	match body with
	| Skip -> ();
	| Seq ss -> List.iter (check_stmt mdesc) ss;
	| UnitCall (edesc,mname,arg_edescs) ->
		(* is the receiver well-defined? *)
		let etype = get_expr_type mdesc edesc in
		(* is this method defined in etype? *)
		let mdesc2 = find_method (find_class etype) mname in
		(* do the arguments match the method's signature? *)
		if (List.length arg_edescs <> mdesc2.number_of_formals) then argumentError ("Wrong number of arguments in call to method"^(unwrap mdesc2.defining_class).class_name^"."^mname)
		else let arg_types = List.map (get_expr_type mdesc) arg_edescs in
   		     let f argtype (Formal (fname,ftype)) = if not (is_subclass argtype ftype) then argumentError ("The expression supplied as argument "^fname^" is not of type "^ftype) else () in
			 List.iter2 f arg_types mdesc2.formals; ();
	| LocalVarDecl (vdesc,vtype) ->
		(* is this variable already declared within the method? *)
		begin try let vd = List.find (fun localvd -> (localvd.variable_name = vdesc.variable_name)) mdesc.locals
			  in variableNameError ("Variable "^vd.variable_name^" is already declared locally twice in method "^methstr)
		with Not_found ->
			(* does this variable use a parameters name? *)
			let ftyper (Formal (fname,ftype)) = ftype in
			try let fm = List.find (fun (Formal (fname,ftype)) -> fname = vdesc.variable_name) mdesc.formals
					in variableNameError ("Variable "^vdesc.variable_name^" is also an argument name in method "^methstr)
			with Not_found ->
				(* is the static type a defined class? *)
				find_class vtype;
				(* annotate the descriptor and add update the method's list of local vars *)
				vdesc.variable_type <- Some vtype;
				vdesc.variable_kind <- Some Local;
				mdesc.locals <- List.append mdesc.locals [vdesc];
		end
	| AssignStmt (vdesc, edesc) ->
		(* does this vdesc correspond to a valid variable? if so annotate and check RHS type *)
		let vdesc0 = try List.find (fun vd -> vd.variable_name = vdesc.variable_name) mdesc.locals 	(* method local? *)
								 with Not_found -> find_instance_var (unwrap mdesc.defining_class) vdesc.variable_name  (* class field? *)
		in vdesc.variable_type <- vdesc0.variable_type; vdesc.variable_kind <- vdesc0.variable_kind; vdesc.offset <- vdesc0.offset;
		Tree.print_vdesc vdesc0;
		Tree.print_vdesc vdesc;
		(* is the RHS a subtype of the LHS? *)
		let etype = get_expr_type mdesc edesc in
		if is_subclass etype (unwrap vdesc.variable_type) then ()
		else semanticError ("Cannot assign expression of type "^etype^" to variable "^vdesc.variable_name^" of type "^(unwrap vdesc.variable_type)^" in method "^methstr)
	| ReturnStmt edesc ->
		let etype = get_expr_type mdesc edesc in
		if is_subclass etype mdesc.return_type then ()
		else semanticError ("Returned expression is of type "^etype^", but "^mdesc.return_type^ " was expected in "^methstr)
	| IfStmt (edesc, thenstmt, elsestmt) ->
		let etype = get_expr_type mdesc edesc in
		if not (is_subclass etype "Boolean") then semanticError ("Condition expression of type Boolean is required in "^methstr)
		else check_stmt mdesc thenstmt; check_stmt mdesc elsestmt
	| WhileStmt (edesc, body2) ->
		let etype = get_expr_type mdesc edesc in
		if (not (is_subclass etype "Boolean")) then (semanticError ("Guard expression of type Boolean is required in "^methstr^", but "^etype^" was given"))
		else check_stmt mdesc body2
	| Newline -> ();;

(* returns true if the sequence of statements has a return statement *)
let rec check_return body =	match body with
	  Seq ss -> List.exists (fun s -> check_return s) ss
	| ReturnStmt _ -> true
	| WhileStmt (_,body2) -> check_return body2
	| IfStmt (_,thenstmt,elsestmt) -> (check_return thenstmt || check_return elsestmt)
	| _ -> false;;

(* check a method *)
let check_method mdesc =
	if (!verbose) then print_string ("checking "^(unwrap mdesc.defining_class).class_name^"."^mdesc.method_name^"...\n");
	(* is the return type Unit? if not, is this r.t. defined? *)
	if (mdesc.return_type <> "Unit") then (find_class mdesc.return_type; ());
	(* check the parameters are well defined *)
	let seen = Hashtbl.create 10 in
	List.iter (fun (Formal (pname,ptype)) ->
		begin
			(* is this name already used by another param? *)
			try let p = Hashtbl.find seen pname in variableNameError ("Parameter name "^pname^" is used twice.")
			with Not_found ->
				(* add param the table of seen names *)
				Hashtbl.add seen pname true;
				(* check type is defined *)
				find_class ptype; ()
		end  ) mdesc.formals;
	(* check the method's body *)
	check_stmt mdesc mdesc.body;
	let cd = unwrap mdesc.defining_class in
	let mstr = cd.class_name^"."^mdesc.method_name in
	(* check the existence/abscence of a return statement *)
	if (mdesc.return_type = "Unit" && check_return mdesc.body) then semanticError ("Method "^mstr^" contains an unexpected return statement.")
	else if (mdesc.return_type <> "Unit" && (not (check_return mdesc.body))) then semanticError ("Method "^mstr^" does not have an expected return statement.") else ();;

(* check the static types of a class' fields are defined
let check_fields cdesc =
	let f vdesc =
		(* inherited variable -> already checked if pcname vname already checked *)
		let vname = vdesc.variable_name and pcname = (unwrap cdesc.parent_desc).class_name in
		if variable_already_checked pcname vname then ()
		else let () = print_string ("checking "^cdesc.class_name^"."^vname^"...\n"); find_class (unwrap vdesc.variable_type); () in
		variable_was_checked cdesc.class_name vname;
	in List.iter f cdesc.variables;; *)

(* check the static types of a class' fields are well-defined *)
let check_fields cdesc =
	let check_field vdesc =
		let vname = vdesc.variable_name and vtype = unwrap vdesc.variable_type in
		if (!verbose && (vtype <> "PRIMITIVE")) then print_string ("checking "^cdesc.class_name^"."^vname^"...\n");
		(* primitive -> don't annotate *)
		if vtype = "PRIMITIVE" then () else let x = find_class vtype in ()
	in List.iter check_field cdesc.variables;;

(* check the methods defined in cdesc
let check_methods cdesc =
	List.iter (fun mdesc ->
		let pname = cdesc.parent_name in
		let cname = cdesc.class_name and mname = mdesc.method_name in
			(* have we checked this method already? if not -> check it! *)
			if (method_already_checked pname mname) then () else check_method mdesc;
			method_was_checked cname mname;
	) cdesc.method_table.methods;; *)

(* check a cdesc's methods *)
let check_methods cdesc =
	let cname = cdesc.class_name in
	List.iter (fun mdesc ->
		let dcname = (unwrap mdesc.defining_class).class_name in
		(* method defined by this class -> annotate, or inherited -> already checked *)
		if cname = dcname then check_method mdesc else ()
	) cdesc.method_table.methods;;

(* check the main sequence of statements *)
let check_main mdesc =
	(* the main 'method' should not return anything *)
	if check_return mdesc.body then semanticError "Main body should not return a value" else
	(* check the body's statements *)
	check_stmt mdesc mdesc.body;;

(** |annotate| -- check ASTs for type errors and flesh out descriptors *)
let annotate (Program (main_mdesc,classDecls)) verboseMode =
	verbose := verboseMode;

	(*** add any library class desriptors descibed in Lib.ml to the Env *)
	add_library_classes (Lib.library_descs ());

	(* set library methods and fields to 'checked'
	List.iter (fun (cname,cdesc) ->
		List.iter (fun mdesc ->
			method_was_checked cname mdesc.method_name
		) cdesc.method_table.methods;
		List.iter (fun vdesc ->
			variable_was_checked cname vdesc.variable_name
		) cdesc.variables;
	) (library_descs ()); *)

	(* fill out the class descriptors and add to the environment *)
	List.iter (fun (ClassDecl (cdesc,fdecls)) -> add_class cdesc fdecls) classDecls;

	let cdescs = List.map (fun (ClassDecl (a,b)) -> a) classDecls in

	if (!verbose) then print_string "-----checking fields-----\n";

	(* check the fields of all classes *)
	List.iter check_fields cdescs;

	if (!verbose) then print_string "-----checking the methods-----\n";

	(* check the methods for each class *)
	List.iter check_methods cdescs;

	if (!verbose) then print_string "-----checking main method-----\n";

	(* check the main method *)
	check_main main_mdesc;;
