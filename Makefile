.PHONY: create-switch
create-switch:
	opam switch create -y . --deps-only

.PHONY: install
install:
	opam update
	opam install -y .

.PHONY: init
init: create-switch install

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: build
build:
	opam exec -- dune build @relude --display short

.PHONY: watch
watch:
	opam exec -- dune build @relude -w

.PHONY: dev-tools
dev-tools:
	opam install . --with-dev-setup

.PHONY: test
test:
	opam exec -- dune runtest

.PHONY: test-watch
test-watch:
	opam exec -- dune runtest -w

.PHONY: test-coverage
test-coverage:
	opam exec -- dune runtest --instrument-with bisect_ppx

.PHONY: docs
docs:
	opam exec -- dune build @doc

.PHONY: docs-serve
docs-serve:
	cd docs && python3 -m http.server 3000

