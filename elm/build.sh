#!/bin/bash
elm make --warn Main.elm
GENERATED_ON=`date +%Y-%m-%d`
sed -i "s/GENERATED_ON_PLACEHOLDER/$GENERATED_ON/" index.html
