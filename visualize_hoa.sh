filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"
./parsehoa $1 > $filename.parsed.hoa
autfilt $filename.parsed.hoa --deterministic --dot > $filename.dot
sed -i 's/|/|\\n/g' $filename.dot
dot -Tpng $filename.dot -o $filename.png
