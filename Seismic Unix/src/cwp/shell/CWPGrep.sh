#! /bin/sh
# /*********************** self documentation **********************/
# Grep  - recursively call egrep in pwd
#
# Usage: Grep [-egrep_options] pattern
#
# Caution:  Do NOT redirect into file in pwd, either use something
#	like  >../Grep.out or perhaps pipe output into mail to yourself.
#
# Author: Jack, 7/95
#
# /**************** end self doc ********************************/
PATH=/bin:/usr/bin:/usr/ucb:
cmd=`basename $0`

BINDIR=$CWPROOT/bin

case $# in
0)  echo "Usage: $cmd [-egrep_options] pattern" 1>&2
    exit 1
esac

for i
do
	case $i in
	-*)
		option="$option $i"
		shift
	;;
	*)
		pattern="$*"
	;;
	esac
done

for i in `ls`
do
	if
		[ -d $i ]
	then
		cd $i
		$BINDIR/Grep $option "$pattern"
		cd ..
	elif
		[ -f $i ]
	then
		if
			egrep $option "$pattern" $i >/dev/null
		then
			echo
			echo "`pwd`/$i:"
			egrep $option "$pattern" $i
		fi
	fi
done

exit 0
