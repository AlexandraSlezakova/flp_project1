build:
	ghc --make src/dka-2-mka.hs src/Parser.hs src/DFAModule.hs src/MDFAModule.hs  -o dka-2-mka

pack:
	zip flp-fun-xsleza20 Makefile -r src/* test/* doc/*

clean:
	@rm -rf src/*.hi src/*.o dka-2-mka *.zip
