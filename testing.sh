#!/bin/bash

echo "TESTING cases 1-14"
out=results.txt
echo "" > $out
for file in tests/*;
do
    echo "${file%.*}" >> $out
    ./nanoml.byte $file | grep "out: *" >> $out
    echo -e "" >> $out
done
