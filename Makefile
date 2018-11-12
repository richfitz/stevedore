PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	Rscript -e 'library(methods); devtools::test()'

test_all:
	REMAKE_TEST_INSTALL_PACKAGES=true make test

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

build_quick:
	R CMD build --no-manual .

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
vignettes_src/%.Rmd: vignettes_src/%.R
	${RSCRIPT} -e 'library(sowsear); sowsear("$<", output="$@")'

vignettes/stevedore.Rmd: vignettes_src/stevedore.Rmd
	cd vignettes_src && ${RSCRIPT} -e 'knitr::knit("stevedore.Rmd")'
	mv vignettes_src/stevedore.md $@
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

vignettes/examples.Rmd: vignettes_src/examples.Rmd
	cd vignettes_src && ${RSCRIPT} -e 'knitr::knit("examples.Rmd")'
	mv vignettes_src/examples.md $@
	sed -i.bak 's/[[:space:]]*$$//' $@
	rm -f $@.bak

vignettes_install: vignettes/stevedore.Rmd vignettes/examples.Rmd
	${RSCRIPT} -e 'library(methods); devtools::build_vignettes()'

vignettes:
	make vignettes_install

pkgdown:
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./scripts/update_web.sh

specs:
	Rscript ./scripts/download_spec.R

.PHONY: all test document install vignettes
