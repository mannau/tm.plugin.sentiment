file = sentiment
#submission = "file"

pdf: latex

all:  Sweave pdf

latex: ${file}.tex ${file}.bib
	texi2dvi -p ${file}.tex

Sweave: ${file}.Rnw
	R CMD Sweave ${file}.Rnw

clean:
	rm -f *~ ${file}.aux ${file}.log ${file}.bbl ${file}.blg ${file}.out ${file}.tpt Rplots.pdf $(file)-* *.Rout *-plot_*.eps *-plot_*.pdf
