all:
	ghc --make src/dka-2-mka.hs src/Parser.hs src/FAModule.hs src/DFAModule.hs  -o dka-2-mka

clean:
	@rm -rf src/*.hi src/*.o dka-2-mka *.zip