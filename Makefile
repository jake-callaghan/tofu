# tofu/Makefile

all: tofuc tofux

include ../keiko/config.mk

TOFUC = print.cmo source.cmo keiko.cmo tree.cmo errors.cmo env.cmo \
  lib.cmo object.cmo integer.cmo boolean.cmo typecheck.cmo \
	object_methods.cmo integer_methods.cmo boolean_methods.cmo \
	kgen.cmo output.cmo lexer.cmo parser.cmo tofuc.cmo

tofuc: $(TOFUC)
	ocamlc -g -o $@ $^

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

KEIKO = ../keiko

tofux: $(KEIKO)/obx-t.a lib.o
	$(CC) $(ALL_RTFLAGS) $^ -o $@

tofuxj: $(KEIKO)/obx-j.a lib.o
	$(CC) $(ALL_RTFLAGS) $^ -o $@

lib.o: lib.c
	$(CC) $(ALL_RTFLAGS) -c $< -o $@

CFLAGS = -g -O2
RTFLAGS = $(CFLAGS) -fno-strict-aliasing
ALL_CFLAGS = $(CFLAGS) -Wall $(INCLUDE)
ALL_RTFLAGS = $(RTFLAGS) -Wall $(INCLUDE)
INCLUDE = -I $(KEIKO)

%.cmi: %.mli
	ocamlc -c -g $<

%.cmo: %.ml
	ocamlc -c -g $<

TESTSRC := $(wildcard test/*.tofu)

test : $(TESTSRC:test/%.tofu=test-%)

test-%: force
	@echo "*** Test $*.tofu"
	./tofuc -O test/$*.tofu >a.k
	-sed -n -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p test/$*.tofu | diff -b - a.k
	$(KEIKO)/pplink -custom -nostdlib lib.k a.k -o a.x >/dev/null
	-./tofux ./a.x >a.test 2>&1
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.tofu | diff - a.test
	@echo "*** Passed"; echo

promote: $(TESTSRC:test/%.tofu=promote-%)

promote-%: force
	./tofuc -O test/$*.tofu >a.k
	$(KEIKO)/pplink -custom -nostdlib lib.k a.k -o a.x >/dev/null
	-./tofux ./a.x >a.test 2>&1
	sed -f promote.sed test/$*.tofu >test/$*.new
	mv test/$*.new test/$*.tofu

force:

MLGEN = parser.mli parser.ml lexer.ml

ML = $(MLGEN) print.ml source.ml keiko.ml lexer.ml tree.ml \
  errors.ml env.ml lib.ml object.ml integer.ml boolean.ml \
	typecheck.ml object_methods.ml integer_methods.ml boolean_methods.ml \
	kgen.ml output.ml parser.ml tofuc.ml

realclean: clean

clean: force
	rm -f *.cmi *.cmo *.o *.output
	rm -f $(MLGEN)
	rm -f tofuc tofux a.out a.k a.x a.test

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

###

parser.cmi : tree.cmi
parser.cmo : tree.cmi keiko.cmi parser.cmi
parser.cmx : tree.cmx keiko.cmx parser.cmi
lexer.cmo : tree.cmi source.cmi parser.cmi
lexer.cmx : tree.cmx source.cmx parser.cmx
print.cmo : print.cmi
print.cmx : print.cmi
source.cmo : print.cmi source.cmi
source.cmx : print.cmx source.cmi
keiko.cmo : tree.cmi source.cmi print.cmi keiko.cmi
keiko.cmx : tree.cmx source.cmx print.cmx keiko.cmi
lexer.cmo : tree.cmi source.cmi parser.cmi
lexer.cmx : tree.cmx source.cmx parser.cmx
tree.cmo : print.cmi keiko.cmi tree.cmi
tree.cmx : print.cmx keiko.cmx tree.cmi
errors.cmo : print.cmi
errors.cmx : print.cmx
env.cmo : tree.cmi errors.cmo env.cmi
env.cmx : tree.cmx errors.cmx env.cmi
lib.cmo : tree.cmi keiko.cmi env.cmi lib.cmi
lib.cmx : tree.cmx keiko.cmx env.cmx lib.cmi
object.cmo : tree.cmi lib.cmi keiko.cmi
object.cmx : tree.cmx lib.cmx keiko.cmx
integer.cmo : tree.cmi object.cmo lib.cmi keiko.cmi
integer.cmx : tree.cmx object.cmx lib.cmx keiko.cmx
boolean.cmo : tree.cmi object.cmo lib.cmi keiko.cmi
boolean.cmx : tree.cmx object.cmx lib.cmx keiko.cmx
typecheck.cmo : tree.cmi lib.cmi errors.cmo env.cmi typecheck.cmi
typecheck.cmx : tree.cmx lib.cmx errors.cmx env.cmx typecheck.cmi
object_methods.cmo : lib.cmi keiko.cmi
object_methods.cmx : lib.cmx keiko.cmx
integer_methods.cmo : lib.cmi keiko.cmi
integer_methods.cmx : lib.cmx keiko.cmx
boolean_methods.cmo : lib.cmi keiko.cmi integer_methods.cmo
boolean_methods.cmx : lib.cmx keiko.cmx integer_methods.cmx
kgen.cmo : tree.cmi object_methods.cmo object.cmo lib.cmi keiko.cmi \
    integer_methods.cmo integer.cmo errors.cmo env.cmi boolean_methods.cmo \
    boolean.cmo kgen.cmi
kgen.cmx : tree.cmx object_methods.cmx object.cmx lib.cmx keiko.cmx \
    integer_methods.cmx integer.cmx errors.cmx env.cmx boolean_methods.cmx \
    boolean.cmx kgen.cmi
output.cmo : tree.cmi print.cmi lib.cmi keiko.cmi errors.cmo output.cmi
output.cmx : tree.cmx print.cmx lib.cmx keiko.cmx errors.cmx output.cmi
parser.cmo : tree.cmi keiko.cmi parser.cmi
parser.cmx : tree.cmx keiko.cmx parser.cmi
tofuc.cmo : typecheck.cmi tree.cmi source.cmi print.cmi parser.cmi \
    output.cmi lib.cmi lexer.cmo kgen.cmi
tofuc.cmx : typecheck.cmx tree.cmx source.cmx print.cmx parser.cmx \
    output.cmx lib.cmx lexer.cmx kgen.cmx
