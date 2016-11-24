#! /bin/sh
# /*********************** self documentation **********************/
# ZAP - kill processes by name
#
# Typical usages:
#	zap ximage
#	zap 'xmovie|xgraph'
#
# Zap accepts full pattern matching for the process names
#
# Caveat: zap assumes that the FIRST field produced by the
#	Unix "ps" command is the pid (process identifier) number.
#	If not, change the number in the awk print statement to
#	the appropriate field.
#
# Author: Jack, 6/95 -- after Kernighan and Pike's zap
#
# /**************** end self doc ********************************/
PATH=/bin:/usr/bin:/usr/ucb
cmd=`basename $0`

case $# in
	1) # OK
;;
	*) echo "Usage: $cmd process_name" 1>&2 ; exit 1
;;
esac

for i in `ps | egrep "$*" | grep -v zap | awk '{print $1}'`
do
	kill -9 $i 2>/dev/null
done

exit 0
