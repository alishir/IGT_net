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
		opts, args = getopt.getopt(sys.argv[1:], "i:o:hs:g:")
	except getopt.GetoptError, err:
		print str(err)
		usage()
		sys.exit(2)
	for o, a in opts:
		if o == "-h":
			usage()
		elif o == "-i":
			input_file = a
		elif o == "-o":
			output_file = a
		elif o == "-s":
			score_sheet_file = a
		elif o == "-g":
			gp_file = a
		else:
			assert False, "unhandled option"
	f = open(input_file, 'r')
	fs = open(score_sheet_file, 'r')
	sub_ans = f.readline().rstrip()
	score_sheet = fs.readlines()[2]
	sub_score = calc_score(sub_ans, score_sheet)
	sub_scales = calc_scales(sub_score)
	
	of = open(output_file, 'w')
	j = 0;
	for scale in 'n e o a c'.split():
		for sub in range(6):
			of.write("%c%d %d\n" % (scale, sub, sub_scales[sub * 5 + j]))
		j = j + 1


	gpf = open(gp_file, 'w')
#	gpf.write("set style data histogram\n");
#	gpf.write("set xtic ()\n");
	j = 0;
	for scale in 'N E O A C'.split():
		gpf.write("%c" % scale);
		for sub in range(6):
			gpf.write(" %d" % sub_scales[sub * 5 + j])
		gpf.write("\n")
		j = j + 1



if __name__ == "__main__":
	main()
