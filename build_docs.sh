#!/bin/sh

SRCS=$(find src/ -name '*.hs')
DIRS=$(find src/ -type d)

# Sources

for file in $SRCS 
do 
	ODIR=$(echo dist/doc/html/ella/`dirname $file`)
	mkdir -p $ODIR
	HsColour -css -anchor $file > $ODIR/`basename $file .hs`.html
done
for DIR in $DIRS
do
	DEST=dist/doc/html/ella/$DIR/hscolour.css
	cp hscolour.css $DEST
done

# Docs

haddock --html --title="Ella" --odir=dist/doc/html/ella   $SRCS \
    --source-module="src/%{MODULE/.//}.html" \
        --source-entity="src/%{MODULE/.//}.html#%{NAME}"
