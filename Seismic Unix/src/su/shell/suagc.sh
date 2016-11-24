#! /bin/sh 
#set -x
# /*********************** self documentation **********************/
# SUAGC - perform agc on SU data 
#
# Note: this is an interface to sugain for backward compatibility
# See selfdoc of:   sugain   for more information
# /**************** end self doc ********************************/

# Author: Jack

# test for CWPROOT
if test "${CWPROOT}" = ""
then
	echo "The environment variable \"CWPROOT\" "
	echo "is not set in the user\'s working shell environment."
	echo "To set this variable in C-shell, use the command: "
	echo "  setenv  CWPROOT  /your/cwp/root/path"
	echo "To set this variable in Bourne or Korn-shell, use the command:"
	echo "  export  CWPROOT=/your/cwp/root/path" ; exit 1

fi

ROOT=${CWPROOT}
BIN=$ROOT/bin

PATH=${PATH}:${BIN}
OLDPROG=sugc
NEWPROG=sugain

#MESSAGE="!!!!!Note: $OLDPROG has been replaced by $NEWPROG!!!!!"

case $# in
	0) 
		$NEWPROG 
		echo $MESSAGE  1>&2 ; exit 1 
	;;
	*)
		$NEWPROG  gagc=1 wagc=.2 $* ; exit 0
	;;
esac

echo $MESSAGE  1>&2 

exit 0
