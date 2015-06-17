SNGFILES = $(wildcard songs/*.sng)

songsbook.pdf: $(SNGFILES)
	./sng2tex -i -t $^ > songsbook.tex
	pdflatex songsbook
	pdflatex songsbook 

examples/firstsong.pdf: examples/firstsong.sng

clean:
	rm -f *.aux *.log
	rm -f songsbook.tex index.tex

cleanall: clean
	rm -f songsbook.pdf

.sng.pdf:
	./sng2tex $^ > /tmp/$*.tex
	pdflatex $! && pdflatex song


.SUFFIXES: .pdf .sng
