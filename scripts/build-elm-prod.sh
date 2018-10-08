#!/bin/bash

if [ ! -e ".git" ]
then
    echo "Call this script from the crate root."
    exit 2
fi

echo "Checking for cli tools ..."

which elm uglifyjs cat wc gzip || exit 2

echo "Cli tools available."

cd gui-src/elm

ELM_JS="../../src/static/elm.js"
#MIN_JS="../../src/static/elm.min.js"
MIN_JS=$ELM_JS

elm make src/Main.elm --output $ELM_JS --optimize

# https://github.com/rtfeldman/elm-spa-example
uglifyjs $ELM_JS --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2' --output=$MIN_JS && uglifyjs $MIN_JS --mangle --output=$MIN_JS

