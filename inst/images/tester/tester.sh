#!/usr/bin/env bash
set -e

if [[ "$#" -ne 1 ]]; then
    echo "Expected one argument"
    exit 1
fi
SRC=$1

if echo $SRC | grep -q "^https://"; then
    DEST=$(mktemp -d)
    git clone $SRC $DEST
    SRC=$DEST
fi

if [ ! -f "$SRC/DESCRIPTION" ]; then
    echo "Did not find a DESCRIPTION" file
    exit 1
fi

PACKAGE=$(grep '^Package:' "$SRC/DESCRIPTION" | \
                 sed -E 's/^Package:[[:space:]]+//')
R CMD build $SRC
PACKAGE_TGZ=$(ls -1tr ${PACKAGE}*gz | tail -n1)

export _R_CHECK_CRAN_INCOMING_=FALSE
Rscript -e "rcmdcheck::rcmdcheck('$PACKAGE_TGZ', args = c('--as-cran', '--no-manual'))"
