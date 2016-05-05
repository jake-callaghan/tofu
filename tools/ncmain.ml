(* ncmain.ml *)

(* The 'nodexp' program is a preprocessor that adds one new notation to
OCaml.  An expression like <LOAD addr_size, <LOCAL stat_link>> is shorthand
for Node (LOAD addr_size, [Node (LOCAL stat_link, [])]) -- a bit easier to
read, but a lot easier to type.  You can also write, e.g., 
<SEQ, @(List.map gen_stmt ss)> as shorthand for the expression
Node (SEQ, List.map gen_stmt ss), which is no longer, but at least gives 
a consistent look to the program.

A program that's written in this notation ought to contain the type
declaration

type optree = Node of inst * optree list

to make the expressions properly typed.  The <...> expressions can be 
used both as patterns and as proper expressions.  Ambiguity with the use of
< and > as comparison operators is removed by the rule that the construct
must begin <ident with no space. *)

open Print

let main () =
  let fname = ref "standard input" in
  let chan = 
    if Array.length Sys.argv < 2 then
      stdin
    else begin
      fname := Sys.argv.(1);
      open_in !fname
    end in
  let lexbuf = Lexing.from_channel chan in
  begin 
    printf "# 1 \"$\"\n" [fStr !fname];
    try Ncparse.text Nclex.token lexbuf with
      Parsing.Parse_error ->
        fprintf stderr "\"$\", line $: syntax error\n" 
          [fStr !fname; fNum !Nclex.line_no];
        exit 1
  end;
  exit 0

let nodexp = main ()
