#!/bin/bash


SYNTH_CODE=synth_code.js 
TONEJS=tonejs.js

cat "$TONEJS" "$SYNTH_CODE" > "out.js"

