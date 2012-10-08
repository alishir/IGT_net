#! /usr/bin/env bash
# Convert bare igt_client to octave file format
for i in `ls *.dat`
do
echo """# Created by To Octave Script 0.0.0
# name: game_seq
# type: matrix
# rows: 1
# columns: 100
""" > $i.oct
cat "$i" | grep -v '#' | head -n 100 |  cut -f1 -d' ' | sed 's/a/97/g' | sed 's/b/98/g' | sed 's/c/99/g' | sed 's/d/100/g' | xargs echo "" >> $i.oct
sed -i '/^$/d' $i.oct
done
