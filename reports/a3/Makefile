SHELL := /bin/bash

.PHONY: open clean

assignment.pdf: assignment.tex
	latexmk -pdf -pdflatex="pdflatex --shell-escape %O %S" assignment.tex

open: assignment.pdf
	open assignment.pdf

run: assignment.pdf
	open assignment.pdf

clean:
	latexmk -C
