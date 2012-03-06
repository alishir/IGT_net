for f in `ls *.gp`
do
	gnuplot "$f"
done
