#! /bin/sh
# /*********************** self documentation **********************/
# UNGLITCH - zonk outliers in data
#
# Usage: unglitch < stdin
#
# Note: this shell just invokes:  sugain < stdin qclip=.99 > stdout
# See selfdoc of:   sugain   for further information
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/unglitch.sh,v $
# $Revision: 1.9 $ ; $Date: 1999/05/12 20:15:48 $

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

CLIPLEVEL=0.99

case $1 in
-)
        echo "Usage: $cmd <stdin" 1>&2; exit 1
;;
esac

case $# in
0)	# Correct usage: cmd <file or ... | cmd ...
	sugain qclip=$CLIPLEVEL
;;
1)	# Also accept usage: cmd filename
	sugain <$1 qclip=$CLIPLEVEL
;;
*)
        echo "Usage: $cmd <stdin" 1>&2; exit 1
;;
esac

exit 0
