SNGFILES = $(wildcard songs/*.sng)
songsbook.pdf: $(SNGFILES)
	./sng2tex $^ > songsbook.tex
	pdflatex songsbook
	pdflatex songsbook

clean:
	rm -f *.aux *.log
	rm -f songsbook.tex index.tex

cleanall: clean
	rm -f songsbook.pdf
