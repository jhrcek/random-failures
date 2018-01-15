#!/bin/bash
ELM_JS=dist/js/elm.js
elm make --warn Main.elm --output=${ELM_JS}
GENERATED_ON=$(date +%Y-%m-%d)
sed -i "s/GENERATED_ON_PLACEHOLDER/${GENERATED_ON}/" ${ELM_JS}
