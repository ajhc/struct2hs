all: build

.PHONY: init build doc test clean install

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

install:
	cabal install

clean:
	rm -rf dist

test: build
	cabal test

doc:
	cabal haddock --hyperlink-source
