for file in src/test/res/specs/*
do
  ./tslsynth "$file" --js --write-hoa "$file".hoa
done
