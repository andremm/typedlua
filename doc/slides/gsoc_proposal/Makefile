all: gsoc_proposal.pdf

gsoc_proposal.pdf: gsoc_proposal.tex
	pdflatex gsoc_proposal.tex
	#bibtex gsoc_proposal
	pdflatex gsoc_proposal.tex
	pdflatex gsoc_proposal.tex

clean:
	rm -f *.aux *.log *.blg *.bbl *.nav *.out *.snm *.toc
