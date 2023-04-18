.PHONY: create-switch
create-switch:
	opam switch create -y . --deps-only

.PHONY: install
install:
	yarn install --legacy-peer-deps
	opam update
	opam install -y .

.PHONY: init
init: create-switch install

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: build
build:
	opam exec -- dune build @relude @test --display short

.PHONY: watch
watch:
	opam exec -- dune build @relude @test -w

.PHONY: dev-tools
dev-tools:
	opam install -y ocamlformat utop ocaml-lsp-server

.PHONY: test
test:
	yarn test

.PHONY: test-coverage
test-coverage:
	yarn test --coverage

.PHONY: docs
docs:
	yarn docs

