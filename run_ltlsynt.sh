filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"
./tsl2tlsf $1 > $filename.tlsf
LTL=$(syfco $filename.tlsf -f ltlxba -m fully)
IN=$(syfco $filename.tlsf --print-input-signals)
OUT=$(syfco $filename.tlsf --print-output-signals)
ltlsynt --formula="$LTL" --ins="$IN" --outs="$OUT" > $filename.hoa
sed -i 1d $filename.hoa
./hoa2code $filename.hoa --python > $filename.code
#./parsehoa $filename.hoa > $filename.parsed.hoa
#autfilt $filename.parsed.hoa --deterministic --dot > $filename.dot
#sed -i 's/|/|\\n/g' $filename.dot
#dot -Tpng $filename.dot -o $filename.png
