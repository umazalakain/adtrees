TMP=tmp
FLAGS=-shell-escape

report.pdf: report.tex bibliography.bib
	mkdir -p $(TMP)
	cp report.tex bibliography.bib $(TMP)
	cd $(TMP) && ghc -i../../src -e ":main bankaccount.dot bankaccount.png" ../examples/BankAccount.hs
	cd $(TMP) && pdflatex $(FLAGS) report.tex && bibtex report.aux && pdflatex $(FLAGS) report.tex && pdflatex $(FLAGS) report.tex
	mv $(TMP)/report.pdf .

clean:
	rm -r $(TMP)
	rm report.pdf
