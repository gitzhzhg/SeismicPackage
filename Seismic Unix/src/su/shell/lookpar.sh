#! /bin/sh
# /*********************** self documentation **********************/
# LOOKPAR - show getpar lines in SU code with defines evaluated
#
# Usage: lookpar filename ...
#
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/lookpar.sh,v $
# $Revision: 1.11 $ ; $Date: 1999/05/12 20:15:48 $

# Note: "cc -E" could be used on many (all?) systems instead of /lib/cpp

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


################################################################################
# test for CWPSRC, use value if set, define as $CWPROOT if not set
# (CWPSRC allows one set of source code and documentation for multiple machines)
################################################################################
if test "${CWPSRC}" = ""
then
CWPSRC=$CWPROOT
fi

ROOT=${CWPROOT}
BIN=$ROOT/bin
SU=$CWPSRC/src/su
I=$ROOT/include

PATH=${PATH}:${BIN}


cmd=`basename $0`

case $# in
0)
	echo "Usage: $cmd file(s)" 1>&2; exit 1
;;
esac

/lib/cpp -I$I $* | grep getpar |
grep -v initgetpar | grep -v "*name" | grep -v "*s"
#grep -v extern | grep -v initgetpar | grep -v "*name" | grep -v "*s"
exit 0
