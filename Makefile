
OPTIONS=-use-ocamlfind
TARGETS=join_locke.native
BINDIR=/usr/local/bin

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

install:
	cp join_locke.native "$(BINDIR)/join_locke"

.PHONY: all clean
