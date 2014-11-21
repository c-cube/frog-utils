
OPTIONS=-use-ocamlfind

LIB_NAMES=frogreduce libfroglock
LIB=$(addprefix $(LIB_NAME), .cma .cmxa .cmxs)
BINARIES=froglock.native frogmap.native
TARGETS=$(LIBS) $(BINARIES)

BINDIR=/usr/local/bin

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

BIN_INSTALL_DIR="$(BINDIR)/froglock/"

install: all
	ocamlfind install frogutils META $(LIBS)
	mkdir -p "$(BIN_INSTALL_DIR)/"
	cp $(BINARIES) "$(BIN_INSTALL_DIR)/"

uninstall:
	ocamlfind remove frogutils
	rm $(addprefix "$(BIN_INSTALL_DIR)/", $(BINARIES))

PHONY: all clean install uninstall
