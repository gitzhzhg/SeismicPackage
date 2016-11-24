#! /bin/sh 
# /*********************** self documentation **********************/
# CPALL , RCPALL - for local and remote directory tree/file transfer
#
# Usage: cpall sourcedir destinationdir 
# Caveat: destinationdir must exist and be writeable by the user
#
# Usage: rcpall sourcedir remotemachine  destinationdir 
#
# If user name is different on the remote machine, then second"
# entry is \"remotemachine -l remoteusername\"
# Caveats: rsh, copy, and write permissions required
#         You must be on the source machine,
#	 destinationdir must exist and be writeable by the user.
#
# Notes: Both of these shell scripts use tar to do the transfer.
#
# /**************** end self doc ********************************/

# Authors: John Stockwell, J.K. Cohen  16 Feb 1990

PATH=/bin:/usr/bin:/usr/ucb
cmd=`basename $0`

case $cmd in
cpall)
	case $# in
		2) # OK
	;;
	*)
		echo "Usage: cpall sourcedir destinationdir"
		echo "destinationdir must exist" 2>&1; exit 1
	;;
	esac
( cd $1 ; tar cf - . ) | ( cd $2 ; tar vxf - )
;;
rcpall)
	case $# in
		3) # OK
	;;
	*)
	echo "Usage: rcpall sourcedir remotemachine  destinationdir" 
	echo " "
	echo "   If user name is different on the remote machine, then second"
	echo "   entry is \"remotemachine -l remoteusername\""
	echo "Caveat: rsh, copy, and write permissions required"
	echo "         You must be on the source machine,"
	echo "	       destinationdir must exist. " 2>&1; exit 1
	;;
	esac
echo "Loading in first block (up to 30 seconds)"
( cd $1 ; tar cBf - . ) | rsh $2 "( cd $3 ; tar vxpBf - )"
;;
esac
exit 0
