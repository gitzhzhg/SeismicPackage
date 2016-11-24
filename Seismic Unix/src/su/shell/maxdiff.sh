#! /bin/sh
# /*********************** self documentation **********************/
# MAXDIFF - find absolute maximum difference in two segy data sets
#
# Usage: maxdiff file1 file2
#
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/maxdiff.sh,v $
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
	sudiff $1 $2 | sumax mode=abs
;;
*)
	echo "Usage: \"$cmd file1 file2\""
;;
esac

exit 0
