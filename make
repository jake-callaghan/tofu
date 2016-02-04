#!/bin/bash
echo "===== BUILDING TOFU COMPILER ====="
rm -f *.cm*
echo "[directory cleaned]"
echo "[beginning compilation]"

ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c *.mli
ocamlc -c *ml
