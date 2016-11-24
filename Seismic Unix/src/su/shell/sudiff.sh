#! /bin/sh 
# /*********************** self documentation **********************/
# SUDIFF, SUSUM, SUPROD, SUQUO, SUPTDIFF, SUPTSUM,
# SUPTPROD, SUPTQUO - difference, sum, product, quotient of two SU data
#                     sets via suop2
#
# Usage:
# sudiff file1 file2 > stdout
# susum file1 file2 > stdout
# ...etc
#
# Note: uses   suop2  to perform the computation
# /**************** end self doc ********************************/

# Author: Jack

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

case $cmd in
sudiff)
	case $# in
	2) suop2 $1 $2 op=diff
	;;
	*) echo "Usage: sudiff file1 file2" 2>&1; exit 1
	;;
	esac
;;
susum)
	case $# in
	2) suop2 $1 $2 op=sum
	;;
	*) echo "Usage: susum file1 file2" 2>&1; exit 1
	;;
	esac
;;
suprod)
	case $# in
	2) suop2 $1 $2 op=prod
	;;
	*) echo "Usage: suprod file1 file2" 2>&1; exit 1
	;;
	esac
;;
suquo)
	case $# in
	2) suop2 $1 $2 op=quo
	;;
	*) echo "Usage: suquo file1 file2" 2>&1; exit 1
	;;
	esac
;;
suptdiff)
	case $# in
	2) suop2 $1 $2 op=ptdiff
	;;
	*) echo "Usage: suptdiff file1 file2" 2>&1; exit 1
	;;
	esac
;;
suptsum)
	case $# in
	2) suop2 $1 $2 op=ptsum
	;;
	*) echo "Usage: suptsum file1 file2" 2>&1; exit 1
	;;
	esac
;;
suptprod)
	case $# in
	2) suop2 $1 $2 op=ptprod
	;;
	*) echo "Usage: suptprod file1 file2" 2>&1; exit 1
	;;
	esac
;;
suptquo)
	case $# in
	2) suop2 $1 $2 op=ptquo
	;;
	*) echo "Usage: suptquo file1 file2" 2>&1; exit 1
	;;
	esac
;;

esac
exit 0
