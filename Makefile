
OPTIONS=-use-ocamlfind

LIB_NAMES=frogutils
LIBS=$(addprefix $(LIB_NAMES), .cma .cmxa .cmxs)
BINARIES=froglock.native frogmap.native frogiter.native frogtptp.native froghop.native
TARGETS=$(LIBS) $(BINARIES)
TEST=foo.native

BINDIR=/usr/local/bin/
SHAREDIR=/usr/local/share/

all:
	ocamlbuild $(OPTIONS) $(TARGETS)

clean:
	ocamlbuild -clean

test: all
	ocamlbuild $(OPTIONS) $(TEST)
	./$(TEST)

INSTALL_TARGETS=$(addprefix _build/src/, $(LIBS))

install_bin: all
	mkdir -p "$(BINDIR)/"
	for b in $(BINARIES) ; do \
	    cp $$b "$(BINDIR)/$$( basename $$b .native )" ; \
	done
	mkdir -p $(SHAREDIR)/frogutils/
	cp data/*.toml $(SHAREDIR)/frogutils/

install_lib: all
	ocamlfind install frogutils META $(INSTALL_TARGETS) _build/src/*.{cmi,cmt}

install_man: all
	for b in $(BINARIES) ; do \
		./$$b --help=groff > $(MANDIR)/man1/$$( basename $$b .native ).1 || true; \
	done

install: install_bin install_lib install_man

uninstall:
	ocamlfind remove frogutils
	for b in $(BINARIES) ; do \
	    rm "$(BINDIR)/$$( basename $$b .native )" ; \
			rm -f "$(MANDIR)/man1/$$( basename $$b .native ).1" ; \
	done
	rm -r "$(SHAREDIR)/frogutils/"
	rm $(addprefix "$(BINDIR)/", $(BINARIES)) || true

PHONY: all clean install uninstall
