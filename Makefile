SNGFILES = $(wildcard beatles/*.sng)
songsbook.pdf: $(SNGFILES)
	./sng2tex $^ > songsbook.tex
	pdflatex songsbook


clean:
	rm -f *.aux *.log *.sxd
	rm -f songsbook.tex

cleanall:
	rm -f songsbook.pdf
