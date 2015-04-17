all:
	ghc --make -o bin/scheme src/main.hs
	./bin/scheme
build:
	ghc --make -o bin/scheme src/main.hs

b:
	ghc --make -o bin/scheme src/main.hs

run:
	./bin/scheme

r:
	./bin/scheme

.PHONY: build run
