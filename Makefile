FILES=`cat main.md`
BASEDIR=$(CURDIR)

pdf:
	pandoc                                                 \
		--verbose                                            \
		-F pandoc-crossref                                   \
		-F pandoc-citeproc                                   \
		--from               markdown                        \
		--to                 latex                           \
		--template           Template/default.tex            \
		--top-level-division chapter                         \
		--latex-engine       xelatex                         \
		--out                thesis.pdf                      \
		metadata.yaml                                        \
		$(FILES)

.PHONY: pdf
