
OPTIONS=-use-ocamlfind
TARGETS=join_locke.native

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

.PHONY: all clean
