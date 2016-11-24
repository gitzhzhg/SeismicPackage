#! /bin/sh
# /*********************** self documentation **********************/
# SUNAME - get name line from self-docs
#
# Usage: suname [name]
#
# Note: dummy selfdocs have been included in all cwp and shell programs
#       that don't have automatic selfdocs.
# /**************** end self doc ********************************/


# $Source: /usr/local/cwp/src/su/shell/RCS/suname.sh,v $
# $Revision: 1.22 $ ; $Date: 1999/05/12 20:15:48 $

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
SRC=${CWPSRC}/src
BIN=$ROOT/bin
SU=$SRC/su
NAMES=$SRC/doc/Headers/Headers.all
DOCS=$SRC/doc/Stripped

PATH=${PATH}:${BIN}


# test to see if user has a preferred PAGER
if test "$PAGER" = ""
	then
		PAGE_PROGRAM=more
	else
		PAGE_PROGRAM=$PAGER
	fi

cmd=`basename $0`

case $# in
0)
	echo " -----  CWP Free Programs -----   "
	echo "CWPROOT=$CWPROOT"
	$PAGE_PROGRAM $NAMES
;;
*)
	grep -i $* $NAMES
;;
esac


echo
echo "To search on a program name fragment, type:" 
echo "     \"suname name_fragment <CR>\""
echo
echo "For more information type: \"program_name <CR>\""
echo
echo "  Items labeled with an asterisk (*) are C programs that may"
echo "  or may not have a self documentation feature."
echo
echo "  Items labeled with a pound sign (#) are shell scripts that may,"
echo "  or may not have a self documentation feature."
echo 
echo " To find information about these codes type:   sudoc program_name"
exit 0

