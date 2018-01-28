#!/bin/bash
ELM_JS=dist/js/elm.js
echo "Generating elm.js"
elm make --warn Main.elm --output=${ELM_JS}

echo "Adding generated on date"
GENERATED_ON=$(date +%Y-%m-%d)
sed -i "s/GENERATED_ON_PLACEHOLDER/${GENERATED_ON}/" ${ELM_JS}

echo "Minifying elm.js"
MINIFIED=dist/js/elm.min.js
uglifyjs ${ELM_JS} --compress --mangle --output ${MINIFIED}
mv ${MINIFIED} ${ELM_JS}
