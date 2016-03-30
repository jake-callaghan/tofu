(* env.mli *)

open Tree
open Hashtbl

(* |env| - an environment cosists of a mapping from class names to pointers to their respective class decriptors *)
type environment = (string, (class_desc ref)) Hashtbl.t

(* returns a new empty environment *)
val newEnvironment : unit -> environment
(* adds a class descriptor to the environment *)
val addClassDescriptor : environment -> string -> class_desc -> unit
(* returns the class_desc for a class in environment *)
val getClassDescriptor : environment -> string -> class_desc