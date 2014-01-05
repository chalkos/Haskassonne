SHELL := /bin/bash
.PHONY: clean draw next play dir_bin dir_doc dir_report all doc report test

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
	$(RM) test/*.next
	$(RM) test/*.play

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

test:
	@echo "Testando Draw"
	@for f in test/*.xml; do \
		if ! bin/draw < $$f > /dev/null; then \
			echo -e "x\n[ERROR] draw < $$f"; \
			continue; \
		fi; \
		if ! bin/draw < $$f | diff - $${f/%.xml/.draw} > /dev/null; then \
			echo -e "x\n[NOK] draw < $$f"; \
			continue; \
		fi; \
		echo -n "."; \
	done
	@echo -e "\nTestando Play"
	@for f in test/*.xml; do \
		if [ "$$f" = "test/008.xml" ] ; then \
			break; \
		fi; \
		for run in {1..50}; do \
			if ! bin/play < $$f > /dev/null; then \
				echo -e "x\n[ERROR] play < $$f"; \
				continue; \
			fi; \
			bin/play < $$f > $${f/%.xml/.play}; \
			xmllint --dtdvalid test/haskassone.dtd $${f/%.xml/.play} 1> /dev/null 2> test/tmp.play; \
			if [[ ! -s test/tmp.play ]] ; then \
				echo -n "."; \
			fi; \
		done; \
	done
	@echo -e "\nTestando Next"
	@for f in test/*.xml; do \
		for run in {1..4}; do \
			if ! bin/next < $$f > /dev/null; then \
				echo -e "x\n[ERROR] next < $$f"; \
				continue; \
			fi; \
			bin/next < $$f > $${f/%.xml/.next}; \
			xmllint --dtdvalid test/haskassone.dtd $${f/%.xml/.next} 1> /dev/null 2> test/tmp.next; \
			if [[ ! -s test/tmp.next ]] ; then \
				echo -n "."; \
			fi; \
		done; \
	done
	@echo ""

