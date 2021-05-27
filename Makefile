GV = $(wildcard *.gv)
PDF := $(GV:.gv=.pdf)

all: run graph render

$(PDF): %.pdf: %.gv
	dot -Tpdf $< -o $@

graph: $(PDF)

show: graph
	zathura graph*.pdf &

render:
	R -e "require(rmarkdown);render('report.rmd');"

run:
	./script.R

.PHONY: graph render all show run
