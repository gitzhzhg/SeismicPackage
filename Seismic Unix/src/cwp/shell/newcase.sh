#! /bin/sh
# /*********************** self documentation **********************/
# NEWCASE - Changes the case of all the filenames in a directory, dir
#
# Usage: newcase -l dir  change all filenames to lower case 
#                -u dir  change all filenames to upper case
#
# Notes: Useful for files downloaded from VAX.
#
#/**************** end self doc ********************************/

# Author: John Stockwell
# Date: 1 June 1990

PATH=/bin:/usr/bin:/usr/ucb
	
case $# in
	2) # OK
	;;
	*)
	echo "Changes the case of all filenames in a directory: dir "
	echo "Usage: newcase -u dir  change all filenames to upper case "
	echo "               -l dir  change all filenames to lower case "
	2>&1; exit 1
;;
esac

for i
do
	case $i in
	-u)
		cd $2
		for OLDNAME in ` ls `
			do
    			NEWNAME=`echo $OLDNAME | tr [a-z] [A-Z]`
			mv $OLDNAME $NEWNAME
			done	
		2>&1; exit 1
	;;
	-l)
		cd $2
		for OLDNAME in ` ls `
			do
    			NEWNAME=`echo $OLDNAME | tr [A-Z] [a-z]`
			mv -i $OLDNAME $NEWNAME
			done	
		2>&1; exit 1
	;;
	-*)
	echo "newcase: Changes the case of all filenames in a directory: dir "
	echo "Usage: newcase -u dir  change to lower case "
	echo "               -l dir  change lower to upper case "
	2>&1; exit 1
	;;
	esac
done

exit 0
