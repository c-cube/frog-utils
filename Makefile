
OPTIONS=-use-ocamlfind

LIB_NAMES=libfroglock
LIBS=$(addprefix $(LIB_NAMES), .cma .cmxa .cmxs)
BINARIES=froglock.native frogmap.native
TARGETS=$(LIBS) $(BINARIES)

BINDIR=/usr/local/bin/

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

INSTALL_TARGETS=$(addprexix _build/src/lock/, $(LIBS))

install: all
	ocamlfind install frogutils META $(INSTALL_TARGETS)
	mkdir -p "$(BIN_INSTALL_DIR)/"
	cp $(BINARIES) "$(BINDIR)/"

uninstall:
	ocamlfind remove frogutils
	rm $(addprefix "$(BINDIR)/", $(BINARIES)) || true

PHONY: all clean install uninstall
