filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"
LTL=$(syfco $1 -f ltlxba -m fully)
IN=$(syfco $1 --print-input-signals)
OUT=$(syfco $1 --print-output-signals)
ltlsynt --formula="$LTL" --ins="$IN" --outs="$OUT" > $filename.hoa
sed -i 1d $filename.hoa
./hoa2code $filename.hoa --arrow > $filename.code
./parsehoa $filename.hoa > $filename.parsed.hoa
autfilt $filename.parsed.hoa --deterministic --dot > $filename.dot
sed -i 's/|/|\\n/g' $filename.dot
dot -Tpng $filename.dot -o $filename.png
