FILES=`cat main.md`
BASEDIR=$(CURDIR)

bib2yaml:
	pandoc-citeproc                                                 \
		--bib2yaml Bibliography/Bibliography.bib                    \
		> Bibliography/Bibliography.yaml

preproccess:
	pandoc                                                          \
		--from               markdown                               \
		--to                 latex                                  \
		--template           Template/pandoc-preamble.tex           \
		--top-level-division chapter                                \
		--latex-engine       xelatex                                \
		--out                Template/processed/pandoc-preamble.tex \
		metadata.yaml                                               \
		$(FILES)

pdf:
	pandoc                                                          \
		-F pandoc-crossref                                          \
		-F pandoc-citeproc                                          \
		-H Template/processed/pandoc-preamble.tex                   \
		-H Template/preamble.tex                                    \
		--from               markdown                               \
		--to                 latex                                  \
		--template           Template/default.tex                   \
		--top-level-division chapter                                \
		--standalone                                                \
		--latex-engine       xelatex                                \
		--out                thesis.pdf                             \
		metadata.yaml                                               \
		$(FILES)

.PHONY: bib2yaml preproccess pdf
