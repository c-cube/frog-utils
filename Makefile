
build:
	@dune build @install

clean:
	@dune clean

test:
	@dune runtest

watch:
	@dune build @all -w

js_links:
	mkdir -p js;
	for i in _build/src/js/*.js ; do ln -sf "$$PWD/$$i" js/ ; done
