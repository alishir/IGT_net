#!/usr/bin/env python

# this code calculate each subject score in all factors of NEO test
# input file is the user choise as a string
# input mapping:
# 		' ' -> very inaccurate
# 		'f' -> inaccurate
# 		'd' -> neither accurate nor inaccurate 
# 		's' -> accurate
# 		'a' -> very accurate

import getopt
import sys
import csv


def get_score(ans, score_type):
	defalut_score = 2
	score = -1
	if score_type == "0":		# see score mapping for meaning of 0 and 1
		if ans == "a":
			score = 4
		elif ans == "s":
			score = 3
		elif ans == "d":
			score = 2
		elif ans == "f":
			score = 1
		elif ans == " ":
			score = 0
		elif ans == "_":
			score = defalut_score;
	elif score_type == "1":
		if ans == "a":
			score = 0
		elif ans == "s":
			score = 1
		elif ans == "d":
			score = 2
		elif ans == "f":
			score = 3
		elif ans == " ":
			score = 4
		elif ans == "_":
			score = defalut_score;
	return score


def calc_score(sub_ans, score_sheet):
	sub_score = []
	for i in range(len(sub_ans)):
		score = get_score(sub_ans[i], score_sheet[i])
		sub_score.append(score)
	return sub_score

def calc_scales(sub_score):
	scales = [0] *  30
	for i in range(len(sub_score)):
		mod = i % 30
		scales[mod] = scales[mod] + sub_score[i]
	return scales

def main():
	try:
		opts, args = getopt.getopt(sys.argv[1:], "i:o:hs:g:l:")
	except getopt.GetoptError, err:
		print str(err)
		usage()
		sys.exit(2)
	for o, a in opts:
		if o == "-h":
			usage()
		elif o == "-i":		# raw input data
			input_file = a
		elif o == "-o":		# csv output of personality vector
			output_file = a
		elif o == "-s":		# score mapping file
			score_sheet_file = a
		elif o == "-g":		# data for gnu plot
			gp_file = a
		elif o == "-l":		# gnuplot script output
			plot_file = a
		else:
			assert False, "unhandled option"

	base_filename = input_file.split('.')[0]
	try:
		output_file
	except NameError:
		output_file = base_filename + '.csv'
	try:
		score_sheet_file
	except NameError:
		score_sheet_file = 'score_map.map'
	try:
		gp_file
	except NameError:
		gp_file = base_filename + '.gpd'
	try:
		plot_file
	except NameError:
		plot_file = base_filename + '.gp'
	
	f = open(input_file, 'r')
	fs = open(score_sheet_file, 'r')
	sub_ans = f.readline().rstrip()
	score_sheet = fs.readlines()[2]
	sub_score = calc_score(sub_ans, score_sheet)
	sub_scales = calc_scales(sub_score)
	
	of = open(output_file, 'w')
	csv_w = csv.writer(of)
	csv_w.writerow(sub_scales)
	of.close()


	gpd = open(gp_file, 'w')
	j = 0;
	for scale in 'N E O A C'.split():
		gpd.write("%c" % scale);
		for sub in range(6):
			gpd.write(" %d" % sub_scales[sub * 5 + j])
		gpd.write("\n")
		j = j + 1
	gpd.close()

	gp = open(plot_file, 'w')
	gp.write("set terminal png nocrop enhanced font verdana 12 size 640,480\n")
	gp.write("set output '%s.png'\n" % base_filename)
	gp.write("set style data histogram\n")
	gp.write("set yrange [0:40]\n")
	gp.write("set xtic ()\n")
	gp.write("set style fill solid 1.0 border -1\n")
	gp.write("set key outside\n")
	gp.write("set title 'subject %s profile'\n" % base_filename)
	gp.write("plot '%s' using 2:xtic(1) ti 'Facet #1', '' u 3 ti 'Facet #2', '' u 4 ti 'Facet #3', '' u 5 ti 'Facet #4', '' u 6 ti 'Facet #5', '' u 7 ti 'Facet #6'\n" % gp_file)
	gp.close()



if __name__ == "__main__":
	main()
