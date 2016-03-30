(* env.ml *)

open Tree
open Hashtbl
open Errors

(* |env| - an environment cosists of a mapping from class names to pointers to their respective class decriptors *)
type environment = (string, (class_desc ref)) Hashtbl.t

(* returns a new empty environment *)
let newEnvironment () = Hashtbl.create 15 	(* the 15 initial size is arbitrary for now *)

(* adds a class descriptor to the environment, if already defined -> throw an exception *)
let addClassDescriptor env cname cdesc = 
	try 
		let x = Hashtbl.find env cname in
		classNameError ("*** Class "^cname^" is defined multiple times ***");
	with 
		Not_found -> Hashtbl.add env cname (ref cdesc);;

(* returns the class_desc for a class in environment *)
let getClassDescriptor env cname = try 
	let r = Hashtbl.find env cname in !r
	with Not_found -> classNameError ("*** Class "^cname^" not found ***");