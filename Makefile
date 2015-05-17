all:
	cabal clean && cabal configure --enable-tests && cabal build && cabal test --show-details=always

build:
	cabal build

configure:
	cabal configure --enable-tests

clean:
	cabal clean

run:
	runhaskell -isrc -itest src/Main.hs

tst:
	cabal build && cabal test --show-details=always

