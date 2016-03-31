(* env.ml *)

open Tree
open Hashtbl
open Vtable
open Errors

(** |def| --  *)

(** |env| -- an environment cosists of a mapping from class names to pointers to their respective class decriptors / vtable pairs *)


(* returns a new empty environment *)
let newEnvironment () = Hashtbl.create 15 	(* the 15 initial size is arbitrary for now *)

