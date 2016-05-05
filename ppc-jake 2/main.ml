(* ppc/main.ml *)

open Print
open Mach
open Source

let debug = ref 0

let usage = "Usage: ppc [-b] [-d n] [-O] file.p"

let spec =
  Arg.align
    ["-b", Arg.Unit (function () -> Kgen.boundchk := true), 
	" enable bound checks";
      "-d", Arg.Int (function x -> debug := x), "n set debug level";
      "-O", Arg.Set Kgen.optflag, " enable peephole optimiser"]

let main () =
  let fns = ref [] in
  Arg.parse spec (function s -> fns := !fns @ [s]) usage;
  if List.length !fns <> 1 then begin
    fprintf stderr "$\n" [fStr usage]; exit 2
  end;
  let in_file = List.hd !fns in
  let in_chan = open_in in_file in
  let lexbuf = Lexing.from_channel in_chan in
  Source.init in_file in_chan;
  Peepopt.debug := !debug;
  ignore (Parsing.set_trace (!debug > 2));

  let prog = 
    try Parser.program Lexer.token lexbuf with
      Parsing.Parse_error -> 
        let tok = Lexing.lexeme lexbuf in
	Source.err_message "syntax error at token '$'" [fStr tok] !Lexer.lineno;
	exit 1 in

  if !debug > 0 then Tree.print_tree stdout "" prog;

  begin try Check.annotate prog with
    Check.Sem_error (fmt, args, ln) ->
      Source.err_message fmt args ln;
      exit 1
  end;

  printf "MODULE Main 0 0\n" [];
  printf "IMPORT Lib 0\n" [];
  printf "ENDHDR\n\n" [];
  Kgen.translate prog;
  printf "! End\n" [];
  exit 0

let ppc = main ()
