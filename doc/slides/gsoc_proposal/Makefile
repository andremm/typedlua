all: gsoc_proposal.pdf

gsoc_proposal.pdf: gsoc_proposal.tex
	pdflatex gsoc_proposal.tex
	#bibtex gsoc_proposal
	pdflatex gsoc_proposal.tex
	pdflatex gsoc_proposal.tex

clean:
	rm -f *.aux *.bbl *.blg *.log *.nav *.out *.snm *.vrb *.toc
