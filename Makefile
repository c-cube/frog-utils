
OPTIONS=-use-ocamlfind

LIB_NAMES=frogutils
LIBS=$(addprefix $(LIB_NAMES), .cma .cmxa .cmxs)
BINARIES=froglock.native frogmap.native
TARGETS=$(LIBS) $(BINARIES)

BINDIR=/usr/local/bin/

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

INSTALL_TARGETS=$(addprefix _build/src/, $(LIBS))

install: all
	mkdir -p "$(BIN_INSTALL_DIR)/"
	cp $(BINARIES) "$(BINDIR)/"
	ocamlfind install frogutils META $(INSTALL_TARGETS) _build/src/*.{cmi,cmt}

uninstall:
	ocamlfind remove frogutils
	rm $(addprefix "$(BINDIR)/", $(BINARIES)) || true

PHONY: all clean install uninstall
