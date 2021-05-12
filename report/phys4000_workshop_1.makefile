ALL_FIGURE_NAMES=$(shell cat phys4000_workshop_1.figlist)
ALL_FIGURES=$(ALL_FIGURE_NAMES:%=%.pdf)

allimages: $(ALL_FIGURES)
	@echo All images exist now. Use make -B to re-generate them.

FORCEREMAKE:

include $(ALL_FIGURE_NAMES:%=%.dep)

%.dep:
	mkdir -p "$(dir $@)"
	touch "$@" # will be filled later.

phys4000_workshop_1-figure0.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure0" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure0.pdf: phys4000_workshop_1-figure0.md5
phys4000_workshop_1-figure1.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure1" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure1.pdf: phys4000_workshop_1-figure1.md5
phys4000_workshop_1-figure2.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure2" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure2.pdf: phys4000_workshop_1-figure2.md5
phys4000_workshop_1-figure3.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure3" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure3.pdf: phys4000_workshop_1-figure3.md5
phys4000_workshop_1-figure4.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure4" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure4.pdf: phys4000_workshop_1-figure4.md5
phys4000_workshop_1-figure5.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure5" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure5.pdf: phys4000_workshop_1-figure5.md5
phys4000_workshop_1-figure6.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure6" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure6.pdf: phys4000_workshop_1-figure6.md5
phys4000_workshop_1-figure7.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure7" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure7.pdf: phys4000_workshop_1-figure7.md5
phys4000_workshop_1-figure8.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure8" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure8.pdf: phys4000_workshop_1-figure8.md5
phys4000_workshop_1-figure9.pdf: 
	pdflatex -halt-on-error -interaction=batchmode -jobname "phys4000_workshop_1-figure9" "\def\tikzexternalrealjob{phys4000_workshop_1}\input{phys4000_workshop_1}"

phys4000_workshop_1-figure9.pdf: phys4000_workshop_1-figure9.md5
