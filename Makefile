all: setup build test lint

.PHONY: setup
setup:
	cabal v2-update
	cabal v2-install hlint weeder
	cabal v2-build --dependencies-only

.PHONY: build
build:
	cabal v2-build

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: test
test: doctest spectest

.PHONY: doctest
doctest:
	cabal v2-test doctest

.PHONY: spectest
spectest:
	cabal v2-test spec

.PHONY: lint
lint:
	hlint app lib
	weeder

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: cabal-fmt
cabal-fmt:
	cabal-fmt my-template.cabal -i
