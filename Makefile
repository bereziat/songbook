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

.sng.tex:
	./sng2tex $< > $@
.crd.tex:
	./crd2tex $< >$@
.tex.pdf:
	pdflatex $<


.SUFFIXES: .pdf .sng .tex .crd
