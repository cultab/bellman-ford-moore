GV = $(wildcard *.gv)
PDF := $(GV:.gv=.pdf)

all: render

$(PDF): %.pdf: %.gv
	dot -Tpdf $< -o $@

graph: run $(PDF)

show: graph
	zathura *graph*.pdf &

render: graph
	R -e "require(rmarkdown);render('report.rmd');"

run:
	./script.R
	./script.R matrix2.txt

clean:
	rm -rf *graph*.gv
	rm -rf *graph*.pdf

submit:
	cp report.pdf 171014_Ευάγγελος_Κατσανδρής_Εργασία_Εξαμήνου.pdf

.PHONY: graph render all show run clean submit
