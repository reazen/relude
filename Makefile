.PHONY: create-switch
create-switch:
	opam switch create -y . --deps-only

.PHONY: install
install:
	yarn install
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
	yarn test

.PHONY: test-watch
test-watch:
	yarn test --watch

.PHONY: test-coverage
test-coverage:
	yarn test --coverage

.PHONY: docs
docs:
	opam exec -- dune build @doc

.PHONY: docs-serve
docs-serve:
	yarn docs

