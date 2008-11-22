#!/bin/sh
DIR=`dirname $0`
runhaskell -i$DIR:$DIR/../src $DIR/Main.hs
