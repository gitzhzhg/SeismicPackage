#! /bin/sh
# /*********************** self documentation **********************/
# SUENV - Instantaneous amplitude, frequency, and phase via: SUATTRIBUTES
# 
# Usage:   suenv < stdin > stdout 
# 
# Note: this shell mimmics the old program SUENV, supersceded by SUATTRIBUTES
# See selfdoc of:   suattributes   for more information
# /**************** end self doc ********************************/

# Author: John Stockwell  1 Sept 1995

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
BIN=${ROOT}/bin
PATH=${PATH}:${BIN}

OLDPROG=suenv
NEWPROG=suattributes

case $# in
	0) 
		$NEWPROG 
	;;
	*)
		for i
		do
			case $i in
			mode=*) 
				mode=`echo $i | sed 's/mode=//g'`
			;;
			unwrap=*) 
				unwrap=`echo $i | sed 's/unwrap=//g'`
			;;
			esac
		done

		if  test "$mode" -a "$unwrap"
		then
			$NEWPROG  mode=$mode unwrap=$unwrap
			echo 1>&2 ; exit 1

		else  if  test "$mode"
		      then
				$NEWPROG mode=$mode
				echo 1>&2 ; exit 1
		      else
				$NEWPROG
				echo 1>&2 ; exit 1
		      fi
		fi
;;
esac

exit 0
