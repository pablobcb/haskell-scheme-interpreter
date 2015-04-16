
build:
	ghc --make -o bin/scheme src/main.hs

run:
	./bin/scheme $(_)

.PHONY: build run
