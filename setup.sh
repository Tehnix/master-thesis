# Install pandoc binaries.
brew install pandoc pandoc-citeproc pandoc-crossref

# Install minimal LaTeX setup.
brew cask install basictex

# Install additional LaTeX packages.
sudo tlmgr update --self
sudo tlmgr install abstract amsfonts amsmath lm ifxetex ifluatex eurosym listings fancyvrb booktabs hyperref ulem geometry setspace babel fontspec mathspec polyglossia tools graphics oberdiek logreq xstring totcount todonotes
sudo tlmgr install collection-fontsrecommended
