#!/usr/bin/env bash

num_sub=$1		# number of subjects
for f in `ls *.txt`
do
	./calc_score.py -i "$f"
done

rm all_data.csv
for f in `ls *.csv -1`
do
	cat "$f" >> all_data.csv
done
