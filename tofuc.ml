(* tofuc.ml *)

open Print
open Source
open Typecheck
open Lib

(* |main| -- main program *)
let main () =
  let dflag = ref true in
  let optflag = ref false in
  let fns = ref [] in
  let usage =  "Usage: ppc [-d] file.p" in
  Arg.parse [("-d", Arg.Set dflag, " Print the tree");
      ("-O", Arg.Unit (fun () -> optflag := true), " Peephole optimiser")]
    (function s -> fns := !fns @ [s]) usage;
  if List.length !fns <> 1 then begin
    fprintf stderr "$\n" [fStr usage]; exit 2
  end;

  let in_file = List.hd !fns in
  let in_chan = open_in in_file in
  Source.init in_file in_chan;
  let lexbuf = Lexing.from_channel in_chan in
  let prog = try Parser.program Lexer.token lexbuf with
      Parsing.Parse_error ->
        let tok = Lexing.lexeme lexbuf in
        err_message "syntax error at token '$'"
          [fStr tok] !Lexer.lineno;
        exit 1 in

  (**** perform the type checking and AST annotations ****)
  Typecheck.annotate prog false;

  (**** generate Keiko code for the AST ****)
  Kgen.translate prog;

  (**** print the resulting AST ****)
  Tree.print_tree stdout prog;

  (**** print the resulting Keiko code ****)
  let Tree.Program(main,cdecls) = prog in
  Keiko.output (main.code);

  (* type checking
  begin try Check.annotate prog with
      Check.Semantic_error (fmt, args, line) ->
        err_message fmt args line;
        exit 1
  end;
  *)
  exit 0

let ppc = main ()
