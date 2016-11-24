#! /bin/sh
# /*********************** self documentation **********************/
# REPLACE - REPLACE string1 with string2  in files
#
# Usage:  replace string1 string2 files
#
#/**************** end self doc ********************************/

# Kernighan and Pike - page 155

BIN=${CWPROOT}/bin
PATH=/bin:/usr/bin:/usr/ucb:$BIN

case $# in
0|1|2)
	echo 'Usage: replace string1 string2 files' 1>&2
	exit 1
esac

left="$1"; right="$2"; shift; shift

for i
do
	$BIN/overwrite $i sed "s@$left@$right@g" $i
done

exit 0
