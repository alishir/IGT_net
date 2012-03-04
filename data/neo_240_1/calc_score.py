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
	n1 = 0
	n2 = 0
	n3 = 0
	n4 = 0
	n5 = 0
	n6 = 0
	e1 = 0
	e2 = 0
	e3 = 0
	e4 = 0
	e5 = 0
	e6 = 0
	o1 = 0
	o2 = 0
	o3 = 0
	o4 = 0
	o5 = 0
	o6 = 0
	a1 = 0
	a2 = 0
	a3 = 0
	a4 = 0
	a5 = 0
	a6 = 0
	c1 = 0
	c2 = 0
	c3 = 0
	c4 = 0
	c5 = 0
	c6 = 0
	for i in range(len(sub_score)):
		mod = i % 30
		if mod == 0:
			n1 = n1 + sub_score[i]
		elif mod == 1:
			e1 = e1 + sub_score[i]
		elif mod == 2:
			o1 = o1 + sub_score[i]
		elif mod == 3:
			a1 = a1 + sub_score[i]
		elif mod == 4:
			c1 = c1 + sub_score[i]
		elif mod == 5:
			n2 = n2 + sub_score[i]
		elif mod == 6:
			e2 = e2 + sub_score[i]
		elif mod == 7:
			o2 = o2 + sub_score[i]
		elif mod == 8:
			a2 = a2 + sub_score[i]
		elif mod == 9:
			c2 = c2 + sub_score[i]
		elif mod == 10:
			n3 = n3 + sub_score[i]
		elif mod == 11:
			e3 = e3 + sub_score[i]
		elif mod == 12:
			o3 = o3 + sub_score[i]
		elif mod == 13:
			a3 = a3 + sub_score[i]
		elif mod == 14:
			c3 = c3 + sub_score[i]
		elif mod == 15:
			n4 = n4 + sub_score[i]
		elif mod == 16:
			e4 = e4 + sub_score[i]
		elif mod == 17:
			o4 = o4 + sub_score[i]
		elif mod == 18:
			a4 = a4 + sub_score[i]
		elif mod == 19:
			c4 = c4 + sub_score[i]
		elif mod == 20:
			n5 = n5 + sub_score[i]
		elif mod == 21:
			e5 = e5 + sub_score[i]
		elif mod == 22:
			o5 = o5 + sub_score[i]
		elif mod == 23:
			a5 = a5 + sub_score[i]
		elif mod == 24:
			c5 = c5 + sub_score[i]
		elif mod == 25:
			n6 = n6 + sub_score[i]
		elif mod == 26:
			e6 = e6 + sub_score[i]
		elif mod == 27:
			o6 = o6 + sub_score[i]
		elif mod == 28:
			a6 = a6 + sub_score[i]
		elif mod == 29:
			c6 = c6 + sub_score[i]

def main():
	try:
		opts, args = getopt.getopt(sys.argv[1:], "i:o:hs:")
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
		else:
			assert False, "unhandled option"
	f = open(input_file, 'r')
	fs = open(score_sheet_file, 'r')
	sub_ans = f.readline().rstrip()
	score_sheet = fs.readlines()[2]
	sub_score = calc_score(sub_ans, score_sheet)
	fw = open(output_file, 'w')
	sub_scales = calc_scales(sub_score)

	for s in sub_score:
		fw.write("%d\n" % s)



if __name__ == "__main__":
	main()
