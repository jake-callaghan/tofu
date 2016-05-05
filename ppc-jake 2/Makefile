# ppc/Makefile

all: ppc ppx

include ../keiko/config.mk

PPC = print.cmo source.cmo util.cmo mach.cmo keiko.cmo dict.cmo \
	tree.cmo lexer.cmo parser.cmo check.cmo peepopt.cmo kgen.cmo \
	main.cmo

ppc: $(PPC)
	ocamlc -g -o $@ $^

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

KEIKO = ../keiko

ppx: $(KEIKO)/obx-t.a lib.o
	$(CC) $(ALL_RTFLAGS) $^ -o $@

ppxj: $(KEIKO)/obx-j.a lib.o
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

TESTSRC := $(wildcard test/*.p)

test : $(TESTSRC:test/%.p=test-%)

test-%: force
	@echo "*** Test $*.p"
	./ppc -O test/$*.p >a.k
	-sed -n -e '1,/^(\*\[\[/d' -e '/^]]\*)/q' -e p test/$*.p | diff -b - a.k
	$(KEIKO)/pplink -custom -nostdlib lib.k a.k -o a.x >/dev/null
	-./ppx ./a.x >a.test 2>&1
	sed -n -e '1,/^(\*<</d' -e '/^>>\*)/q' -e p test/$*.p | diff - a.test
	@echo "*** Passed"; echo

promote: $(TESTSRC:test/%.p=promote-%)

promote-%: force
	./ppc -O test/$*.p >a.k
	$(KEIKO)/pplink -custom -nostdlib lib.k a.k -o a.x >/dev/null
	-./ppx ./a.x >a.test 2>&1
	sed -f promote.sed test/$*.p >test/$*.new
	mv test/$*.new test/$*.p

force:

MLGEN = parser.mli parser.ml lexer.ml 

ML = $(MLGEN) check.ml check.mli dict.ml dict.mli \
	lexer.mli mach.ml mach.mli main.ml keiko.mli keiko.ml \
	print.ml print.mli source.ml source.mli kgen.mli kgen.ml tree.ml \
	tree.mli util.ml peepopt.mli peepopt.ml

realclean: clean

clean: force
	rm -f *.cmi *.cmo *.o *.output
	rm -f $(MLGEN)
	rm -f ppc ppx a.out a.k a.test

depend: $(ML) force
	(sed '/^###/q' Makefile; echo; ocamldep $(ML)) >new
	mv new Makefile

###

parser.cmi: tree.cmi keiko.cmi dict.cmi
parser.cmo: tree.cmi lexer.cmi keiko.cmi dict.cmi parser.cmi
parser.cmx: tree.cmx lexer.cmx keiko.cmx dict.cmx parser.cmi
lexer.cmo: util.cmo source.cmi print.cmi parser.cmi keiko.cmi dict.cmi \
    lexer.cmi
lexer.cmx: util.cmx source.cmx print.cmx parser.cmx keiko.cmx dict.cmx \
    lexer.cmi
check.cmo: tree.cmi print.cmi mach.cmi lexer.cmi keiko.cmi dict.cmi check.cmi
check.cmx: tree.cmx print.cmx mach.cmx lexer.cmx keiko.cmx dict.cmx check.cmi
check.cmi: tree.cmi print.cmi
dict.cmo: print.cmi mach.cmi keiko.cmi dict.cmi
dict.cmx: print.cmx mach.cmx keiko.cmx dict.cmi
dict.cmi: print.cmi mach.cmi keiko.cmi
lexer.cmi: parser.cmi keiko.cmi dict.cmi
mach.cmo: mach.cmi
mach.cmx: mach.cmi
mach.cmi:
main.cmo: tree.cmi source.cmi print.cmi peepopt.cmi parser.cmi mach.cmi \
    lexer.cmi kgen.cmi check.cmi
main.cmx: tree.cmx source.cmx print.cmx peepopt.cmx parser.cmx mach.cmx \
    lexer.cmx kgen.cmx check.cmx
keiko.cmi: print.cmi
keiko.cmo: print.cmi keiko.cmi
keiko.cmx: print.cmx keiko.cmi
print.cmo: print.cmi
print.cmx: print.cmi
print.cmi:
source.cmo: print.cmi source.cmi
source.cmx: print.cmx source.cmi
source.cmi: print.cmi
kgen.cmi: tree.cmi
kgen.cmo: tree.cmi print.cmi peepopt.cmi mach.cmi lexer.cmi keiko.cmi \
    dict.cmi kgen.cmi
kgen.cmx: tree.cmx print.cmx peepopt.cmx mach.cmx lexer.cmx keiko.cmx \
    dict.cmx kgen.cmi
tree.cmo: print.cmi keiko.cmi dict.cmi tree.cmi
tree.cmx: print.cmx keiko.cmx dict.cmx tree.cmi
tree.cmi: keiko.cmi dict.cmi
util.cmo:
util.cmx:
peepopt.cmi: keiko.cmi
peepopt.cmo: util.cmo source.cmi print.cmi keiko.cmi peepopt.cmi
peepopt.cmx: util.cmx source.cmx print.cmx keiko.cmx peepopt.cmi
