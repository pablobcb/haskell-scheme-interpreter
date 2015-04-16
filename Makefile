
build:
	ghc --make -o bin/scheme src/main.hs

run:
	./bin/scheme

.PHONY: build run
