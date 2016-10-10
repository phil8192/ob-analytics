PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

man-docs:
        R -e 'if(!require("devtools")) install.packages("devtools")'\
	R -e 'devtools::document()'

html-docs:
        R -e 'if(!require("rmarkdown")) install.packages("rmarkdown")'\
	R -e 'rmarkdown::render("vignettes/guide.Rmd", "html_document", output_dir="/tmp")'

pdf-docs:
        R -e 'if(!require("rmarkdown")) install.packages("rmarkdown")'\
	R -e 'rmarkdown::render("vignettes/guide.Rmd", "pdf_document", output_dir="/tmp")'

build: man-docs
	cd ..;\
	R CMD build $(PKGSRC)

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

install: build
	cd ..;\
        R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	$(RM) -rf inst/doc
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

