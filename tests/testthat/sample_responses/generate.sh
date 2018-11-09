#!/usr/bin/env bash
set -e
if [ $# -eq 2 ]; then
    V_OLD=$1
    V_NEW=$2
    FROM=$1
    TO=$2
else
    echo "Wrong number of args"
    exit 1
fi

DEST="v$V_NEW"
SRC="v$V_OLD"

if [ -d $DEST ]; then
    echo "Directory $DEST already exists"
    exit 1
fi

cp -r "$SRC" "$DEST"

ls -1 "$DEST"/*.json | sed 's;.*/;- [ ] ;' > "$DEST/rewrite.md"

FILES=$(find $DEST -name '*.R')
FROM="version: $V_OLD"
TO="version: ${V_NEW}"
for f in $FILES; do
    sed -i.bak "s/$FROM/$TO/g" $f
done

rm "$DEST"/*.json "$DEST"/*.bak
sed "s/$V_OLD/$V_NEW/" "../test-spec-responses-${V_OLD}.R" > \
    "../test-spec-responses-${V_NEW}.R"
