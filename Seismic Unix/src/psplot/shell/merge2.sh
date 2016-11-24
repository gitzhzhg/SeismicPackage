#! /bin/sh
# /*********************** self documentation **********************/
# MERGE2 - put 2 standard size PostScript figures on one page
#
# Usage: merge2 fig1 fig2
#
# Notes: Translation values are hard-coded numbers that work well for 
#	standard size (8.5 x 11) figures. 
# See selfdoc of:   psmerge for details
#/**************** end self doc ********************************/

# Author: Craig Artley

BINDIR=${CWPROOT}/bin

cmd=`basename $0`

# set defaults
MODE=landscape
ASPECT=true
STACKED=true

case $cmd in
	merge2v)	MODE=portrait; ASPECT=max; STACKED=true ;;
esac

for i in $*; do
     case $1 in
	m*=*)	MODE=`echo $1 | sed s/m.*=//`; shift ;;
	a*=*)	ASPECT=`echo $1 | sed s/a.*=//`; shift ;;
	s*=*)	STACKED=`echo $1 | sed s/s.*=//`; shift ;;
	*)	break ;;
     esac
done

case $# in
    2) # check that input files exist
	if [ ! -f $1 -o ! -f $2 ]
		then echo "$cmd:  psfiles $1 and/or $2 not available!" 1>&2
		exit 1
	fi
	;;
    *) # echo some documentation
	echo
	echo "MERGE 2 PostScript figures onto one page" 1>&2
	echo
	echo "Usage: $cmd [options] psfile psfile > psfile" 1>&2
	echo
	echo "options:" 1>&2
	echo "  mode=landscape	=portrait " | tr -d "\012" 1>&2
	echo "to change page orientation" 1>&2
	echo "  aspect=true		=max " | tr -d "\012" 1>&2
	echo "to maximize figure size but distort aspect ratio" 1>&2
	echo "  stacked=true		=false " | tr -d "\012" 1>&2
	echo "to put figures side by side" 1>&2
	echo
	echo "Notes:  The landscape mode ignores the " | tr -d "\012" 1>&2
	echo "aspect= and stacked= flags." 1>&2
	echo "        Options may be abbreviated, " | tr -d "\012" 1>&2
	echo "so m=p is equivalent to mode=portrait." 1>&2
	exit 1
	;;
esac

case $MODE in
    p*)	break ;;
    *)	# landscape mode
    	${BINDIR}/psmerge in=$1 rotate=90 scale=0.707,0.707 translate=8.25,0.0 \
		in=$2 rotate=90 scale=0.707,0.707 translate=8.25,5.0
	exit 0 ;;
esac

case $ASPECT in
    m*)	# unequal scaling to maximize figure size
	case $STACKED in
	    f*)
	    	# figures side-by-side
		${BINDIR}/psmerge in=$1 scale=0.5,1.0 translate=0.5,0.0 \
			in=$2 scale=0.5,1.0 translate=4.25,0.0
		exit 0 ;;
	    *)
	    	# figures stacked
		${BINDIR}/psmerge in=$1 scale=1.0,0.5 translate=0.0,5.25 \
			in=$2 scale=1.0,0.5 translate=0.0,0.25
		exit 0 ;;
	esac ;;

    *)	# equal scaling to preserve aspect ratio
	case $STACKED in
	    f*)
	    	# figures side-by-side
		${BINDIR}/psmerge in=$1 scale=0.5,0.5 translate=0.50,2.25 \
			in=$2 scale=0.5,0.5 translate=4.25,2.25
		exit 0 ;;
	    *)
	    	# figures stacked
		${BINDIR}/psmerge in=$1 scale=0.5,0.5 translate=2.25,5.25 \
			in=$2 scale=0.5,0.5 translate=2.25,0.25
		exit 0 ;;
	esac ;;
esac

echo "$cmd:  This can't happen.  Complain to Craig."
exit 1
