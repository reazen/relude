.PHONY: create-switch
create-switch:
	opam switch create -y . --deps-only

.PHONY: install
install: ## Install development dependencies
	yarn install --legacy-peer-deps
	opam update
	opam install -y .