#! /bin/sh
# /*********************** self documentation **********************/
# RECIP - sum opposing (reciprocal) offsets in cdp sorted data
#
# Usage: recip <stdin >stdout
#
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/recip.sh,v $
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
tmp=/usr/tmp/$$.$cmd

# Arrange to remove tmp at shell termination (0) or signal
# Internal trap to ignore 0 is to avoid double remove in case of signal
trap "rm -f $tmp; trap '' 0; exit 1" 0 1 2 3 15

case $1 in
-)
    	echo "Usage: $cmd <stdin >stdout" 1>&2; exit 1
;;
esac

case $# in
0)	# Correct usage: cmd <file >stdout or ... | cmd ...
	suabshw >$tmp
	susort cdp offset <$tmp | surecip
;;
1)	# Also accept usage: cmd filename
	suabshw <$1 >$tmp
	susort cdp offset <$tmp | surecip
;;
*)
    	echo "Usage: $cmd <stdin >stdout" 1>&2; exit 1
;;
esac

exit 0
