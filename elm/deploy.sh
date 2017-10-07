#!/usr/bin/env bash
DEPLOYMENT_DIR=../../jhrcek.github.io/random-failures
rm --recursive --force $DEPLOYMENT_DIR
cp --recursive dist $DEPLOYMENT_DIR

echo "Minifying js file"
rm $DEPLOYMENT_DIR/js/elm.js
uglifyjs dist/js/elm.js --compress --mangle --output $DEPLOYMENT_DIR/js/elm.js
