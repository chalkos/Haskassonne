.PHONY: clean draw next play dir_bin dir_doc dir_report

FLAGS ?= -O -isrc/ -o

dir_bin:
	@if [ ! -d bin ]; then mkdir bin; fi;

dir_doc:
	@if [ ! -d doc ]; then mkdir doc; fi;

dir_report:
	@if [ ! -d report ]; then mkdir report; fi;

clean: dir_report dir_doc dir_bin
	@echo "  (remover ficheiros temporários e binários)"
	$(RM) src/*.o
	$(RM) src/*.hi
	$(RM) bin/*

bin/draw: dir_bin
	@echo "  (compilando Draw)"
	ghc $(FLAGS) $@ src/Draw.hs

draw: bin/draw

bin/next: dir_bin
	@echo "  (compilando Next)"
	ghc $(FLAGS) $@ src/Next.hs

next: bin/next

bin/play: dir_bin
	@echo "  (compilando Play)"
	ghc $(FLAGS) $@ src/Play.hs

play: bin/play
