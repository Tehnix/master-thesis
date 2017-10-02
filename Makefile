FILES=`cat main.md`
BASEDIR=$(CURDIR)

preproccess:
	pandoc                                                 \
		--from               markdown                        \
		--to                 latex                           \
		--template           Template/pandoc-preamble.tex    \
		--top-level-division chapter                         \
		--latex-engine       xelatex                         \
		--out                Template/proccessed-preamble.tex\
		metadata.yaml                                        \
		$(FILES)

pdf:
	pandoc                                                 \
		-F pandoc-crossref                                   \
		-F pandoc-citeproc                                   \
		-H Template/proccessed-preamble.tex                  \
		-H Template/preamble.tex                             \
		--from               markdown                        \
		--to                 latex                           \
		--template           Template/default.tex            \
		--top-level-division chapter                         \
		--latex-engine       xelatex                         \
		--out                thesis.pdf                      \
		metadata.yaml                                        \
		$(FILES)

.PHONY: preproccess pdf
