
OPTIONS=-use-ocamlfind

LIB_NAMES=frogutils
LIBS=$(addprefix $(LIB_NAMES), .cma .cmxa .cmxs)
BINARIES=froghop.native froglock.native frogiter.native frogmap.native frogtptp.native
TARGETS=$(LIBS) $(BINARIES)

BINDIR=/usr/local/bin/
SHAREDIR=/usr/local/share/

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

INSTALL_TARGETS=$(addprefix _build/src/, $(LIBS))

install_bin: all
	mkdir -p "$(BIN_INSTALL_DIR)/"
	for b in $(BINARIES) ; do \
	    cp $$b "$(BINDIR)/$$( basename $$b .native )" ; \
	done
	mkdir -p $(SHAREDIR)/frogutils/
	cp data/*.toml $(SHAREDIR)/frogutils/

install_lib: all
	ocamlfind install frogutils META $(INSTALL_TARGETS) _build/src/*.{cmi,cmt}

install: install_bin install_lib

uninstall:
	ocamlfind remove frogutils
	for b in $(BINARIES) ; do \
	    rm "$(BINDIR)/$$( basename $b .native )" ; \
	done
	rm -r "$(SHAREDIR)/frogutils/"
	rm $(addprefix "$(BINDIR)/", $(BINARIES)) || true

PHONY: all clean install uninstall
