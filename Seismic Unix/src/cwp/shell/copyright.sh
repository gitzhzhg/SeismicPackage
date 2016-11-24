#! /bin/sh
# /*********************** self documentation **********************/
# COPYRIGHT - insert CSM COPYRIGHT lines at top of files in working directory
#
# Usage: copyright file(s)
#
# /**************** end self doc ********************************/

# $Author: john $
# $Source: /NeXTMount_3.1b/usr/local/cwp/src/cwp/shell/RCS/copyright.sh,v $
# $Revision: 1.8 $ ; $Date: 93/06/17 13:44:35 $

ROOT=${CWPROOT}
BIN=$ROOT/bin
PATH=/bin:/usr/bin:/usr/ucb:${BIN}
tmpfile=/tmp/$$.cright
THISYEAR=`this_year`

for i in `ls`
do
	if
		[ -d $i ]
	then
		cd $i
		$BIN/copyright
		cd ..
	elif # C files
		[ .c = `echo $i | sed 's/.*\.c/.c/'` -o .h = `echo $i |sed 's/.*\.h/.h/'` ]
	then
		>$tmpfile
	echo "/* Copyright (c) Colorado School of Mines, $THISYEAR.*/" >>$tmpfile
	echo "/* All rights reserved.                       */" >>$tmpfile
		echo "" >>$tmpfile
		cat $i >>$tmpfile

		mv -f $tmpfile $i
	elif # Fortran files
		[ .f = `echo $i | sed 's/.*\.f/.f/'` ]
	then
		>$tmpfile
	echo "* Copyright (c) Colorado School of Mines, $THISYEAR." >>$tmpfile
		echo "* All rights reserved." >>$tmpfile
		echo "" >>$tmpfile
		cat $i >>$tmpfile

		mv -f $tmpfile $i
	fi
done

exit 0
