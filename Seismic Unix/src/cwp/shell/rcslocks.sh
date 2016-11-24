#! /bin/sh
# locks - list rcs files that are locked
# Usage: locks
# Jack K. Cohen, 1987

PATH=/bin:/usr/bin:/usr/ucb

for i in RCS/*,v
do
	LINE=`head -5 $i | awk 'NR==5 {print $2}'`

	if [ "$LINE" = "strict;" ]
	then
		echo $i: $LINE
	fi
done

exit 0
