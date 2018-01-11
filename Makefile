PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

index:
	${RSCRIPT} -e 'stevedore:::write_spec_index("inst/spec")'

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

test_all:
	REMAKE_TEST_INSTALL_PACKAGES=true make test

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"

README.md: README.Rmd
	Rscript -e "options(warnPartialMatchArgs=FALSE); knitr::knit('$<')"
	sed -i.bak 's/[[:space:]]*$$//' README.md
	rm -f $@.bak

## We can't build vignettes on CRAN and systems without docker (and
## even when they do, it's not a great idea because we build and
## remove a bunch of containers etc.
# vignettes/%.Rmd: vignettes/src/%.R
# 	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'

vignettes/src/%.Rmd: vignettes/src/%.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'

vignettes/stevedore.Rmd: vignettes/src/stevedore.Rmd
	cd vignettes/src && ${RSCRIPT} -e 'knitr::knit("stevedore.Rmd")'
	mv vignettes/src/stevedore.md $@
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

vignettes_install: vignettes/stevedore.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

vignettes:
	rm -f vignettes/stevedore.Rmd
	make vignettes_install

pkgdown:
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./update_web.sh

.PHONY: all test document install vignettes
