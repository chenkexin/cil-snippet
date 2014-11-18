CC	:= gcc -save-temps
CFLAGS	:= -Wall -Werror

OCAMLFLAGS := 
OCAMLCPACKAGES := -package unix,str,cil

OCAMLOPTFLAGS := $(OCAMLFLAGS)
OCAMLOPTPACKAGES := $(OCAMLCPACKAGES)
OCAMLOPTINCS := $(OCAMLCINCS)
OCAMLOPTLIBS := -linkpkg

LIBOPENSSLDIR := /home/ckx/pj_ssl/openssl-1.0.1f
all: test ciltest1

test: main.o test1.o
	$(CC) $(CFLAGS) $^ -o $@

ciltest1: ciltest1.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTINCS) $(OCAMLOPTPACKAGES) $(OCAMLOPTLIBS) $^ -o $@

run-ciltest1: ciltest1 test1.i main.i
	./ciltest1

ciltest2: ciltest2.cmx
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTINCS) $(OCAMLOPTPACKAGES) $(OCAMLOPTLIBS) $^ -o $@
run-ciltest2: ciltest2 test1.i main.i
	./ciltest2


openssl-files:
	find $(LIBOPENSSLDIR) -name '*.i' | \
		egrep '/(crypto)/' > $@

.c.i:
	$(CPP) $< > $@

#
.mli.cmi:
	ocamlfind ocamlc $(OCAMLFLAGS) $(OCAMLCINCS) $(OCAMLCPACKAGES) -c $<
.ml.cmo:
	ocamlfind ocamlc $(OCAMLFLAGS) $(OCAMLCINCS) $(OCAMLCPACKAGES) -c $<
.ml.cmx:
	ocamlfind ocamlopt $(OCAMLOPTFLAGS) $(OCAMLOPTCINCS) $(OCAMLOPTPACKAGES) -c $<

clean: 
	rm -f *.s *~ *.bak core *.cmi *.cmo *.cmx *.cma *.o *.so *.a *.i ciltest1 ciltest2 test 
.SUFFIXES: .c .i .cmo .cmi .cmx .ml .mli
.PHONY: run-ciltest1 clean

