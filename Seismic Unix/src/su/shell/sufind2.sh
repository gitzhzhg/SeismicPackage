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
# /**************** end self doc ********************************/

# $Source: /usr/local/cwp/src/su/shell/RCS/sufind2.sh,v $
# $Revision: 1.1 $ ; $Date: 1999/05/12 22:15:38 $

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
SRC=${CWPSRC}/src
DOC=$SRC/doc/Stripped

PATH=${PATH}:${BIN}


cmd=`basename $0`

case $# in
0)
	echo "$cmd - get info from self-docs about SU programs"
    	echo "Usage: $cmd [-v -n] string" 1>&2
	echo "\"$cmd string\" gives brief synopses" 1>&2
	echo "\"$cmd -v string\" verbose hunt for relevant items" 1>&2
	echo "\"$cmd -n name_fragment\" searches for command name" 1>&2
	exit 1
;;
esac

case $1 in
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
		grep -i "$1" * 
	;;
	esac
;;
-n)
	shift
	string=`echo $1 | tr "[a-z]" [A-Z]"`
	cd $DOC
	echo ""
	grep "$string" * | sed "s/^.*$1.*://"
;;
*)
	cd $DOC
	echo ""
	if
		grep -i "$1" * >/dev/null
	then
		for i in `grep -li "$1" *`
		do
			cat $i | sed 4q
			echo ""
		done
	fi
;;
esac

echo ""
echo "For more information type: \"sudoc program_name <CR>\""
exit 0
