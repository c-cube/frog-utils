
OPTIONS=-use-ocamlfind

LIB_NAMES=frogutils
LIBS=$(addprefix $(LIB_NAMES), .cma .cmxa .cmxs)
BINARIES=froglock.native frogmap.native frogiter.native
TARGETS=$(LIBS) $(BINARIES)

BINDIR=/usr/local/bin/

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

INSTALL_TARGETS=$(addprefix _build/src/, $(LIBS))

install: all
	mkdir -p "$(BIN_INSTALL_DIR)/"
	for b in $(BINARIES) ; do \
	    cp $$b "$(BINDIR)/$$( basename $$b .native )" ; \
	done
	ocamlfind install frogutils META $(INSTALL_TARGETS) _build/src/*.{cmi,cmt}

uninstall:
	ocamlfind remove frogutils
	for b in $(BINARIES) ; do \
	    rm "$(BINDIR)/$$( basename $b .native )" ; \
	done
	rm $(addprefix "$(BINDIR)/", $(BINARIES)) || true

PHONY: all clean install uninstall
