#! /bin/sh
# /*********************** self documentation **********************/
# SUBAND - Trapezoid-like Sin squared tapered Bandpass filter via  SUFILTER
# 
# Usage:   suband < stdin > stdout 
# 
# Note: this shell mimmics the old program SUBAND, supersceded by SUFILTER
# See selfdoc of:   sufilter   for more information
# /**************** end self doc ********************************/

# Author: John Stockwell  10 April 1992

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
OLDPROG=suband
NEWPROG=sufilter
#MESSAGE="!!!!!Note: $OLDPROG has been replaced by $NEWPROG!!!!!"


case $# in
	0) 
		$NEWPROG 
		echo $MESSAGE  1>&2 ; exit 1 
	;;
	*)
		for i
		do
			case $i in
			f1=*) 
				f1=`echo $i | sed 's/f1=//g'`
			;;
			f2=*) 
				f2=`echo $i | sed 's/f2=//g'`
			;;
			f3=*) 
				f3=`echo $i | sed 's/f3=//g'`
			;;
			f4=*) 
				f4=`echo $i | sed 's/f4=//g'`
			;;
			dt=*) 
				dt=`echo $i | sed 's/dt=//g'`
			;;
			esac

		done
		if  test "$f1" -a "$f2" -a "$f3" -a "$f4" -a "$dt"
		then
			$NEWPROG f=$f1,$f2,$f3,$f4 dt=$dt 
			echo 1>&2
			echo $MESSAGE  1>&2  ; exit 1
		else 	if  test "$f1" -a "$f2" -a "$f3" -a "$f4" 
			then
				$NEWPROG f=$f1,$f2,$f3,$f4  
				echo 1>&2
				echo $MESSAGE  1>&2  ; exit 1
			else
				echo 1>&2
				echo "suband -- Bad filter parameters" 1>&2
				echo 1>&2
				echo $MESSAGE  1>&2  ; exit 1
			fi
		fi
;;
esac

echo $MESSAGE  1>&2 

exit 0
