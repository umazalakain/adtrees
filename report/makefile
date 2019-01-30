TMP=tmp

report.pdf: report.tex bibliography.bib
	mkdir -p $(TMP)
	pdflatex -output-directory $(TMP) report.tex
	cp bibliography.bib $(TMP) && cd $(TMP) && bibtex report.aux
	pdflatex -output-directory $(TMP) report.tex
	pdflatex -output-directory $(TMP) report.tex
	mv $(TMP)/report.pdf .

clean:
	rm -r $(TMP)
