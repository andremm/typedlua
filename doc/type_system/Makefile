all: type_system.pdf

type_system.pdf: type_system.tex
	pdflatex type_system.tex
	#bibtex type_system
	pdflatex type_system.tex
	pdflatex type_system.tex

clean:
	rm -f *.aux *.log *.blg *.bbl
