(** tofu/object_methods.ml *)
(* this module contains the Keiko code for the methods of the Object class *)

open Lib
open Keiko

(* tests if the addresses are the same *)
let isEqual_code =
  let tlab = label () and exitLab = label () in
  SEQ [
    LOCAL 20; LOADW;    (* push 'that' address *)
    LOCAL 16; LOADW;    (* push 'this' address *)
    JUMPC (Eq,tlab);           (* equal -> tlab *)
    gen_boolean false;  (* push addr of new Boolean(false) *)
    JUMP exitLab;
    LABEL tlab;
    gen_boolean true;   (* push addr of new Boolean(false) *)
    LABEL exitLab;
    RETURNW ]
