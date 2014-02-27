all: type_system.pdf old.pdf

type_system.pdf: type_system.tex
	pdflatex type_system.tex
	#bibtex type_system
	pdflatex type_system.tex
	pdflatex type_system.tex

old.pdf: old.tex
	pdflatex old.tex
	#bibtex old 
	pdflatex old.tex
	pdflatex old.tex

clean:
	rm -f *.aux *.log *.blg *.bbl
