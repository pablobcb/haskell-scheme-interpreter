all:
	cd src && ghc --make -o ../bin/scheme main.hs && cd ..
	./bin/scheme
build:
	ghc --make -o bin/scheme src/main.hs

run:
	./bin/scheme

.PHONY: build run
