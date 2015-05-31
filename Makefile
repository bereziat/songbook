SNGFILES = $(wildcard songs/*.sng)
songsbook.pdf: $(SNGFILES)
	./sng2tex $^ > songsbook.tex
	pdflatex songsbook
	songs-2.15/src/songidx/songidx general.sxd general.sbx
	pdflatex songsbook

clean:
	rm -f *.aux *.log *.sxd
	rm -f songsbook.tex

cleanall:
	rm -f songsbook.pdf
