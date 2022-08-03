#!/bin/bash

# a script to run ltlsynt on the command line by passing in a .tlsf file

filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"
LTL=$(syfco $1 -f ltlxba -m fully)
IN=$(syfco $1 --print-input-signals)
OUT=$(syfco $1 --print-output-signals)
ltlsynt --formula="$LTL" --ins="$IN" --outs="$OUT" --hoaf="i" --simplify="bwoa-sat" > $filename.hoa
