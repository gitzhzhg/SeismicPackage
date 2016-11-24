#! /bin/sh
# /*********************** self documentation **********************/
# DIRTREE - show DIRectory TREE
#
# Usage: dirtree
#
# /**************** end self doc ********************************/

# John Stockwell May 1992 
# (based loosely on a shell of the same name by Jack K. Cohen, 1988)

ROOT=${CWPROOT}
BIN=$ROOT/bin

SPACE="\_"
SPACER="___"
SPACING=$1

for i in `ls`
do
	if
		[ -d $i ]
	then
		echo
	fi
	echo $SPACING$SPACE$i
	if
		[ -d $i ]
	then
		OLDSPACING=$SPACING
		SPACING=$SPACING$SPACER
		cd $i
		$BIN/dirtree $SPACING
		cd ..
		SPACING=$OLDSPACING
	fi
done
exit 0
