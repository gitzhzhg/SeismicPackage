#! /bin/sh 
# /*********************** self documentation **********************/
# SUFIND - get info from self-docs
#
# Usage: sufind [-v -n] string
#
# sufind string    gives a brief synopsis
# sufind -v string  is a verbose hunt for relevant items
# sufind -n name_fragment      searches for command name
#
# Author: CWP: Jack K. Cohen,  1992
# Modified by: CWP: S. Narahara, 04/11/1998.
#
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/sufind.sh,v $
# $Revision: 1.17 $ ; $Date: 2005/02/03 22:12:24 $

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

if [ "`uname`" = "SunOS" ]
   then
   AWK=/usr/bin/nawk
else
   AWK=awk
fi

ROOT=${CWPROOT}
BIN=$ROOT/bin
SRC=$ROOT/src
DOC=$SRC/doc/Stripped

PATH=${PATH}:${BIN}

GREP=egrep

cmd=`basename $0`

case $# in
0)
	echo ""
	echo "$cmd - get info from self-docs about SU programs"
    	echo "Usage: $cmd [-v -n -P<command_pattern>] string" 1>&2
	echo "	(\"string\" can be an \"egrep\" pattern)" 1>&2
	echo "\"$cmd string\" gives brief synopses" 1>&2
	echo "\"$cmd -v string\" verbose hunt for relevant items" 1>&2
	echo "\"$cmd -n name_fragment\" searches for command name" 1>&2
	echo "\"$cmd -P<pattern> string\"" 1>&2
	echo "	gives brief synopses by searching for \"string\" among" 1>&2
	echo "	commands/libraries whose names match \"egrep\" pattern" 1>&2
	echo ""
	exit 1
;;
esac

tty=0; tty -s && tty=1
cmd_pattern=.\*
sufind_command()
{
	cd $DOC
	echo ""

	### echo "$cmd_pattern"; exit
	files=`ls | $GREP -e "$cmd_pattern"`
	### echo $files; exit

	if echo $files | $GREP "${cmd_pattern}\*" > /dev/null; then
		echo 'NO DOC FILES ???'
	else
		for i in $files; do
			[ $tty = 1 ] && ${AWK} -v f="$i" \
				'BEGIN{printf("%-40s\r",f);exit}' >/dev/tty
			found=`$GREP -li "$1" "$i"`
			[ $tty = 1 ] && ${AWK} \
				'BEGIN{printf("%-40s\r"," ");exit}' >/dev/tty
			if [ "$found" != "" ]; then
				echo "+"
				cat "$i" | sed 4q
			fi
		done
	fi
}

case "$1" in
-v)
	shift
	case $# in
	0)
		echo "Need a string" 1>&2
		echo "Usage: $cmd [-v -n] string" 1>&2; exit 1
	;;
	*)
		cd $DOC
		echo ""
		$GREP -i "$1" * 
	;;
	esac
;;
-n)
	shift
	string=`echo $1 | tr "[a-z]" "[A-Z]"`
	cd $DOC
	echo ""
	$GREP "$string" * | sed "s/^.*$1.*://"
;;
-P*)
	cmd_pattern=`echo "$1" | sed s/-P//`
	shift
	case $# in
	0)
		echo "Need a string" 1>&2
		echo "Usage: $cmd -P<command_pattern> string" 1>&2; exit 1
	;;
	*)
		sufind_command "$1"
	;;
	esac
;;
*)
	sufind_command "$1"
	echo 
	echo 
	echo "SUNAME listings containing the pattern: $1 "
	suname | grep "$1"
;;
esac

echo ""
echo "For more information type: \"sudoc program_name <CR>\""
exit 0
