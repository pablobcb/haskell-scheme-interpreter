
build:
	ghc -o bin/scheme --make src/main.hs

run:
	./bin/scheme $(arg1)

.PHONY: build run
