all: typedlua.pdf

typedlua.pdf: typedlua.tex
	pdflatex typedlua.tex
	bibtex typedlua
	pdflatex typedlua.tex
	pdflatex typedlua.tex

clean:
	rm -f *.aux *.log *.blg *.bbl
