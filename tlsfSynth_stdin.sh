#!/bin/bash

# a script to run ltlsynt on the command line by passing in a .tlsf file

input=$(cat)
echo $input > tmp.tlsf
LTL=$(syfco tmp.tlsf -f ltlxba -m fully)
IN=$(syfco tmp.tlsf --print-input-signals)
OUT=$(syfco tmp.tlsf --print-output-signals)
RESULT=$(ltlsynt --formula="$LTL" --ins="$IN" --outs="$OUT" --hoaf="i" --simplify="bwoa-sat")
if [[ $RESULT == U* ]]
then 
  echo "UNREALIZABLE"
else
  echo "REALIZABLE"
fi
