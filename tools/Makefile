# tools/Makefile

all : nodexp sshconfig pibake

nodexp : print.cmo nclex.cmo ncparse.cmo ncmain.cmo
	ocamlc -o $@ $^

nclex.ml : nclex.mll
	ocamllex nclex.mll

ncparse.mli ncparse.ml : ncparse.mly
	ocamlyacc -v ncparse.mly

DIR := $(shell pwd)

pibake: pibake.in sshconfig
	sed 's:@DIR@:$(DIR):g' $< >$@
	chmod +x $@

sshconfig: sshconfig.in
	sed 's:@DIR@:$(DIR):g' $< >$@

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

MLGEN = nclex.ml ncparse.mli ncparse.ml

ML = $(MLGEN) ncmain.ml print.mli print.ml

clean : force
	rm -f nodexp *.cmo *.cmi *.output $(MLGEN)
	rm -f sshconfig pibake

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

force:

###

nclex.cmo: ncparse.cmi
nclex.cmx: ncparse.cmx
ncparse.cmi:
ncparse.cmo: print.cmi ncparse.cmi
ncparse.cmx: print.cmx ncparse.cmi
ncmain.cmo: print.cmi ncparse.cmi nclex.cmo
ncmain.cmx: print.cmx ncparse.cmx nclex.cmx
print.cmi:
print.cmo: print.cmi
print.cmx: print.cmi
