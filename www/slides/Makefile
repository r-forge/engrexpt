R_PROG = R
DRIVERS = ch01 ch02 ch03 ch04 ch05 ch06 ch07 ch08 ch09 ch10
RNWSRC = ch01 ch02 ch03 ch04 ch05 ch06 ch07 ch08 ch09 ch10
TEXSRC = 
SWEAVE = SweaveSlides.sty SweaveSlides.cfg

all: $(DRIVERS:=D.pdf) $(DRIVERS:=-4.pdf) $(RNWSRC:_R.txt)
	${RM} $(DRIVERS:H.pdf) *.out Rplots.ps figs/*.pdf figs/*.eps \
          *.aux *.log *.toc *.nav *.snm *.vrb *.dvi

allR: $(RNWSRC:=.R)
	touch allR

allT: $(RNWSRC:=_R.txt)
	touch allT

%.tex: %.Rnw
	${R_PROG} CMD Sweave $<

%.R: %.Rnw
	${R_PROG} CMD Stangle $<

%.pdf : %.tex
	${R_PROG} CMD texi2pdf $<

%.dvi : %.tex
	TEXINPUTS=${TPUTS} texi2dvi $<
#	texclean $<

clean:
	$(RM) $(RNWSRC:=.tex) \
          $(RNWSRC:=.R) Rplots.ps *.out allR figs/*.pdf figs/*.eps \
          *.aux *.log *.toc *.nav *.snm *.vrb *.dvi

dist:
	$(RM) Rplots.ps $(RNWSRC:=.tex) $(DRIVERS:=H.tex) $(DRIVERS:=D.tex) \
	   $(DRIVERS:=-4.tex) $(DRIVERS:=-4a.tex) *.out allR \
	   figs/*.pdf figs/*.eps *.aux *.log *.toc *.nav *.snm *.vrb *.dvi

%_R.txt: %.R
	cp $< $@

%D.tex: %.tex
	@echo "\\documentclass[dvipsnames,pdflatex,beamer]{beamer}" >> $@
	@echo "\\input{"$*"}" >> $@
	@echo "\\end{document}" >> $@

%H.tex: %.tex
	@echo "\\documentclass[dvipsnames,pdflatex,handout]{beamer}" >> $@
	@echo "\\input{"$*"}" >> $@
	@echo "\\end{document}" >> $@

%H.pdf: %H.tex
	TEXINPUTS=${TPUTS} texi2dvi --pdf $<
#	texclean $<

%D.pdf: %D.tex
	TEXINPUTS=${TPUTS} texi2dvi --pdf $<
#	texclean $<

%-4.tex: %H.pdf
	@echo "\\documentclass[letterpaper,landscape]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\\\begin{document}" >> $@
	@echo "\\includepdf[nup=2x2,pages=-]{"$*"H.pdf}" >> $@
	@echo "\\end{document}" >> $@

%-4.pdf: %-4.tex
	texi2dvi --pdf $<
#	texclean $<

%-2.tex: %H.pdf
	@echo "\\documentclass[letterpaper]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\\\begin{document}" >> $@
	@echo "\\includepdf[nup=1x2,pages=-]{"$*"H.pdf}" >> $@
	@echo "\\end{document}" >> $@

%-2.pdf: %-2.tex
	texi2dvi --pdf $<
#	texclean $<


# A4 paper version
%-4a4.tex: %H.pdf
	@echo "\\documentclass[a4paper,landscape]{article}" > $@
	@echo "\\usepackage{pdfpages}" >> $@
	@echo "\\begin{document}" >> $@
	@echo "\\includepdf[nup=2x2,pages=-]{"$*"H.pdf}" >> $@
	@echo "\\end{document}" >> $@

%-4a4.pdf: %-4.tex
	texi2dvi --pdf $<
#	texclean $<
