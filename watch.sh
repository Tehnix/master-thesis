watchman-make \
    -p 'Chapter/*.md' \
    -t 'pdf' \
    -p 'Template/*.tex' \
    -t 'preproccess pdf' \
    -p 'Bibliography/*.bib' \
    -t 'bib2yaml' \
