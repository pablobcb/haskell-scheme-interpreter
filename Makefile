all:
	cabal clean && cabal configure --enable-tests && cabal build && cabal test --show-details=always

build:
	cabal build

configure:
	cabal configure --enable-tests

clean:
	cabal clean

t:
	cabal test --show-details=always

