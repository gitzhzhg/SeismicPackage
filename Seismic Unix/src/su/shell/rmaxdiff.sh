#! /bin/sh
# /*********************** self documentation **********************/
# RMAXDIFF - find percentage maximum difference in two segy data sets
#
# Usage: rmaxdiff file1 file2
#
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/rmaxdiff.sh,v $
# $Revision: 1.8 $ ; $Date: 1999/05/12 20:15:48 $

# test for CWPROOT
if test "${CWPROOT}" = ""
then
	echo "The environment variable \"CWPROOT\" "
	echo "is not set in the user's working shell environment."
	echo "To set this variable in C-shell, use the command: "
	echo "  setenv  CWPROOT  /your/cwp/root/path"
	echo "To set this variable in Bourne or Korn-shell, use the command:"
	echo "  export  CWPROOT=/your/cwp/root/path" ; exit 1

fi

ROOT=${CWPROOT}
BIN=$ROOT/bin
PATH=${PATH}:${BIN}

cmd=`basename $0`

case $# in
2)
	echo "max on first, maxdiff and percent:"
	denom=`sumax mode=abs <$1`
	num=`sudiff $1 $2 | sumax mode=abs`
	per=`percent $num $denom`
	echo $denom , $num , $per
;;
*)	# includes "cmd -"
	echo "Usage is \"rmaxdiff file1 file2\""
	echo
	echo "This shell can be used to compare the su data from"
	echo "a \"new\" and \"old\" version of an suprogram when the"
	echo "changes have affected the floating point operation"
	echo "order, so that the outputs are shown to differ by \"cmp\"."
	echo "If rmaxdiff produces an answer less than about 7.0e-5,"
	echo "one has some evidence that the changes have not harmed"
	echo "the program."
;;
esac

exit 0
