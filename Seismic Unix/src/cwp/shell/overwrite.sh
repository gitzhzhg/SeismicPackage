#! /bin/sh
# /*********************** self documentation **********************/
# OVERWRITE - copy stdin to stdout after EOF
#
# This shell is called from the shell script:    replace
#
# /**************** end self doc ********************************/


# Kernighan and Pike - page 154

opath=$PATH
PATH=/bin:/usr/bin:/usr/ucb

case $# in
0|1)
	echo 'Usage: overwrite file cmd [args]' 1>&2
	exit 2
esac

file=$1; shift
new=./overwr.$$
trap 'rm -f $new; exit 1' 1 2 15

if
	PATH=$opath "$@" >$new
then
	trap '' 1 2 15
	cp $new $file
else
	echo "overwrite: $1 failed, $file unchanged" 1>&2
	exit 1
fi

rm -f $new
exit 0
