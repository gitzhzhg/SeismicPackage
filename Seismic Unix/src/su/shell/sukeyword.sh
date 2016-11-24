#! /bin/sh
# /*********************** self documentation **********************/
# SUKEYWORD -- guide to SU keywords in segy.h
#
# Usage: sukeyword -o            to begin at the top of segy.h
#        sukeyword [string]      to find [string]
#
# Note:  keyword=  occurs in many SU programs.
# /**************** end self doc ********************************/

# test for CWPROOT
if test "${CWPROOT}" = ""
then
        echo "sukeyword has to access the segy.h file, so it needs"
        echo " to have the environment variable \"CWPROOT\" set."
        echo
        echo "To set this variable in C-shell, use the command:"
        echo "  setenv  CWPROOT  /your/cwp/root/path"
        echo "To set this variable in Bourne or Korn-shell, use the command:"
        echo "  export  CWPROOT=/your/cwp/root/path"
        echo "It is best to put the command in respectively"
        echo "your .login or .profile once and for all."; exit 1

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
SU=${CWPSRC}/src/su

PATH=${PATH}:${BIN}

cmd=`basename $0`

# test to see if user has a preferred PAGER
if test "$PAGER" = ""
        then
                PAGE_PROGRAM=more
        else
                PAGE_PROGRAM=$PAGER
fi


# now check cases
case $# in
	1) # OK
	;;
        *) 
                set "selfdoc"
        ;;
esac

case $1 in
        -o)
                set "tracl"
        ;;
        -*)
                set "selfdoc"
        ;;
esac

case $1 in
	selfdoc)
		echo 1>&2
		echo "SUKEYWORD -- guide to SU keywords in segy.h" 1>&2
		echo 1>&2
		echo "Usage: $cmd -o       to begin at the top of segy.h" 1>&2
		echo "Usage: $cmd [string] to find [string] " 1>&2
	;;
	*)
		start=$1\;
                exec $PAGE_PROGRAM +/$start $SU/include/segy.h
        ;;
esac

exit 0
