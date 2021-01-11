all: setup build test lint

.PHONY: setup
setup:
	cabal v2-update
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

.PHONY: bench
bench:
	cabal v2-bench

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: lint
lint:
	hlint app lib

.PHONY: format
format: format-src format-cabal

.PHONY: format-cabal
format-cabal:
	cabal-fmt my-template.cabal -i

.PHONY: format-src
format-src:
	stylish-haskell -ir app
	stylish-haskell -ir lib
	stylish-haskell -ir test
	stylish-haskell -ir benchmark

