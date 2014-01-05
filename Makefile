.PHONY: clean draw next play dir_bin dir_doc dir_report all doc report

FLAGS ?= -fprof-auto -O -isrc/ -o

all: report doc draw next play

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
	$(RM) roundAntes.draw
	$(RM) roundDepois.draw
	$(RM) roundNext.xml
	$(RM) roundPlay.xml
	$(RM) report/report.aux
	$(RM) report/report.log
	$(RM) report/report.toc
	$(RM) report/report.out

report: report/report.pdf

report/report.pdf: dir_report
	@echo "  (gerando Relatório)"
	cd report; pdflatex report.tex; pdflatex report.tex

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

doc: dir_doc
	@echo "  (gerando documentacao)"
	cd src; haddock -h -o ../doc/ ArtASCII.hs Escritor.hs Leitor.hs Pontuar.hs Tabuleiro.hs FakePrettyShow.hs
