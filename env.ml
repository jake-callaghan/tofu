(* env.ml *)

open Tree;;
open Hashtbl;;
open Errors;;

(** |environment| -- a mapping from type names to class_descs in the AST *)
type environment = (string, class_desc) Hashtbl.t

(** |env| -- the semantic environment maintained throughout type-checking *)
let env = ref (Hashtbl.create 100);;

(* return the number of class descriptors in the current env *)
let size () = Hashtbl.length !env;;

let print () = Hashtbl.iter (fun n cd -> print_string n; print_string " ") !env; print_newline();;

(* return a deep-copy of some data structure *)
let (copy : 'a -> 'a) =
  fun value ->
    Marshal.from_string
      (Marshal.to_string value [Marshal.Closures])
      0;;

(* unwrap an optional type *)
let unwrap o = match o with
| Some v -> v
| _ -> compilationError "Some v was expected but None was found!";;

(*********************************)
(* variable descriptor functions *)
(*********************************)

(** |add_instance_var| -- add vdesc to cdesc's variables *)
let add_instance_var cdesc vdesc =
	let vdescs = cdesc.variables in
	let vname = vdesc.variable_name in
	(* inherited / same name variable already? *)
	try let vd = List.find (fun vd1 -> vd1.variable_name = vname) vdescs
		in variableNameError ("Variable "^vname^" is defined multiple times.");
	with Not_found ->
		cdesc.variables <- (List.append vdescs [vdesc]);;

(** |find_instance_var| -- return a vdesc corresponding to field vname in class corresponding to cdesc *)
let find_instance_var cdesc vname =
	let vdescs = cdesc.variables in
	try List.find (fun vd -> vd.variable_name = vname) vdescs
    with Not_found -> variableNameError ("Varibale "^cdesc.class_name^"."^vname^" not found.");;

(*******************************)
(* method descriptor functions *)
(*******************************)

(** |add_method| -- add mdesc to cdesc's vtable,
  * overriding any previous mdescs with the same name
  * this allows for a subclass to override any method (all methods are virtual)
  *)
let add_method cdesc mdesc =
  let vt = cdesc.method_table in
	 (* here we either replace an overriden method desc, or simply append a fresh one to the vt *)
    let rec insert mds = match mds with
  	| [] -> [mdesc]
  	| (x::xs) -> if x.method_name = mdesc.method_name then mdesc :: xs else x :: insert xs
  in cdesc.method_table.methods <- insert (vt.methods);
  mdesc.defining_class <- Some cdesc;; (* annotate the mdesc with new defining class name *)

(** |find_method| -- try to extract a method descriptor with matching name in cdesc
  * raising Not_found expception if not found
  *)
let find_method cdesc mname =
  let vt = cdesc.method_table in
  try List.find (fun md -> (md.method_name = mname)) vt.methods
  with Not_found -> methodNameError ("Method "^cdesc.class_name^"."^mname^" not found.");;

(************************)
(* class_decl functions *)
(************************)

(** |add_features| -- add a list of feature declerations to class descriptor cdesc *)
let rec add_features cdesc fdecls = match fdecls with
	  [] -> ();
	| (InstanceVarDecl (vdesc,vtype)) :: tail ->
		vdesc.variable_type <- Some vtype;
		vdesc.variable_kind <- Some Field;
		add_instance_var cdesc vdesc;
		add_features cdesc tail;
	| (MethDecl mdesc) :: tail ->
		add_method cdesc mdesc;
		add_features cdesc tail;;

(** |find_class| -- returns the class descriptor for class cname in env *)
let find_class cname =
	try Hashtbl.find !env cname
	with Not_found -> classNameError ("Class "^ cname ^" not found.");;

(** |add_class| -- adds a class descriptor and its features to environment env *)
let add_class cdesc fdecls =
	let cname = cdesc.class_name in
	(* class already defined? *)
	try let x = Hashtbl.find !env cname in classNameError ("Class "^ x.class_name ^ " is defined multiple times.")
	with Not_found ->
		(* add descriptor to hash table *)
		Hashtbl.add !env cname cdesc;
		(* find parent class' descriptor *)
		let pdesc = find_class cdesc.parent_name in
		(* set pointer to this descriptor *)
		let () =
			cdesc.parent_desc <- Some pdesc in
		(* inherit methods in a new vtable *)
		let vt =
			pdesc.method_table.methods in
		let () =
			cdesc.method_table.methods <- (copy vt) in
		(* inherit instance vars *)
		let vars =
			pdesc.variables in
		let () =
			cdesc.variables <- (copy vars) in
		(* process the features defined in this class *)
		add_features cdesc fdecls;;

(** |is_subclass| -- returns true iff cname1 a subclass of cname2 in env *)
let is_subclass cname1 cname2 =
	let cdesc0 = find_class cname1 in
	let rec search cd =
		if cd.class_name = cname2 then true else
		if cd.class_name = "Object" then false else
		search (unwrap cd.parent_desc)
	in search cdesc0;;
