(** tofu/output.ml *)
(* this module provides methods for outputting Keiko code along with assembler directives *)

open Tree
open Print
open Errors

let unwrap o = match o with
| Some x -> x
| _ -> compilationError "some v was expected, but none existed - during Output ";;

(** |output_methods| -- prints out the appropriate procedure directives for all methods *
                      * defined in this class *
                      * and the vtable for all methods present with appropriate names *)
let output_methods cdesc =
  (* --methods-- *)
  let cname = cdesc.class_name in
  let vt_name = "%"^cname in
  let mds = cdesc.method_table.methods in
  let output_method md =
    let defcname = (unwrap md.defining_class).class_name in
    if defcname = cdesc.class_name then begin
      let size = 4 * (List.length md.locals) in
      let fmname = defcname ^ "." ^ md.method_name in
      printf "PROC $ $ 0 0\n" [fStr fmname; fNum size];
      Keiko.output md.code;
      printf "END\n\n" [] ;
      end in
  (* print out the method headers and code *)
  List.iter output_method mds;
  (* --vtable-- *)
  printf "DEFINE $\n" [fStr vt_name];
  List.iter (fun md ->
    let defcname = (unwrap md.defining_class).class_name in
    printf "WORD $\n" [fStr (defcname^"."^md.method_name)]
  ) cdesc.method_table.methods;
  printf "\n" [];;

(** |output| -- write the compiler output to file str.k *)
let output (Program(main_desc,classDecls)) =
  let cdescs = List.map (fun (ClassDecl(cd,fd)) -> cd) classDecls in

  printf "MODULE Main 0 0\n" [];
  printf "IMPORT Lib 0\n" [];
  printf "ENDHDR\n\n" [];

  (* --output all the library code-- *)
  List.iter output_methods (List.map (fun (a,b) -> b) (Lib.library_descs ()));
  (* --output all the procedure code and vtables of user-defined classes-- *)
  List.iter output_methods cdescs;

  (* --output the main method-- *)
  printf "PROC MAIN 0 0 0\n" [];
  (* code *)
  Keiko.output main_desc.mdesc.code;
  printf "END\n\n" [];
  (* declare variables in main method *)
  Keiko.output main_desc.decls;
  printf "! END\n" [];;
