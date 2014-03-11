PDFLATEX  = pdflatex

MANUAL = manual_v4.tex

MODELS = model_packages.tex

all: 	$(MANUAL) $(MODELS)
	$(PDFLATEX) $(MANUAL)
	$(PDFLATEX) $(MANUAL)
	$(PDFLATEX) $(MODELS)
	$(PDFLATEX) $(MODELS)
	
clean:; rm *.aux *.log *.out *.toc *.bbl *.blg *.dvi *.ps
	
realclean:; rm *.aux *.log *.out *.toc *.pdf *.bbl *.blg *.dvi *.ps
