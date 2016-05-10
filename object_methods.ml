(** tofu/object_methods.ml *)
(* this module contains the Keiko code for the methods of the Object class *)

open Lib
open Keiko

(* calls the print primitive *)
let print_code () = SEQ [
    LOCAL 16; LOADW;    (* push 'this' address *)
    CONST 0;            (* static link *)
    GLOBAL "_print_num";
    PCALL 1
]

(* tests if the addresses are the same *)
let isEqual_code () =
  let tlab = label () and exitLab = label () in SEQ [
    LOCAL 20; LOADW;    (* push 'that' address *)
    LOCAL 16; LOADW;    (* push 'this' address *)
    JUMPC (Eq,tlab);    (* equal -> tlab *)
    gen_boolean false;  (* push addr of new Boolean(false) *)
    JUMP exitLab;
    LABEL tlab;
    gen_boolean true;   (* push addr of new Boolean(false) *)
    LABEL exitLab;
    RETURNW ]

(* a list of method name * code pairs, to speed up pairing of method code with the method descriptors *)
let object_methods_code = [("print",print_code); ("isEqual",isEqual_code)]
