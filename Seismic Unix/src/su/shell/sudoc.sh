#! /bin/sh
# /*********************** self documentation **********************/
# SUDOC - get DOC listing for code
#
# Usage: sudoc name
#
# Note: Use this shell script to get selfdoc information for
# codes labeled with and asterisk (*) or pound sign (#) in suname list      
# /**************** end self doc ********************************/
#set -x


# $Source: /usr/local/cwp/src/su/shell/RCS/sudoc.sh,v $
# $Revision: 1.14 $ ; $Date: 2012/03/21 19:05:56 $

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
SRC=${CWPSRC}/src
BIN=$ROOT/bin
STRIP=$SRC/doc/Stripped

PATH=${PATH}:${BIN}


# test to see if user has a preferred PAGER
if test "$PAGER" = ""
	then
		PAGE_PROGRAM=more
	else
		PAGE_PROGRAM=$PAGER
	fi


case $# in
1) #OK

	cd ${STRIP}

	NAME=`echo $1 | tr [A-Z] [a-z]`

	SHELLS="cwp/shell par/shell psplot/shell su/shell "

	LIBS=" cwp/lib par/lib psplot/lib xplot/lib tri/lib Xtcwp/lib  \
                Xmcwp/lib su/lib "

	MAINS="cwp/main par/main psplot/main xplot/main \
	        Xtcwp/main Xmcwp/main su/graphics/psplot \
		comp/dct/main comp/dwpt/1d comp/dwpt/2d \
		xtri tetra/main  Mesa/main \
		su/main/amplitudes  \
		su/main/attributes_parameter_estimation  \
		su/main/convolution_correlation su/main/data_conversion \
		su/main/datuming su/main/decon_shaping \
		su/main/dip_moveout su/main/filters su/main/headers \
		su/main/headers su/main/interp_extrap \
		su/main/migration_inversion su/main/multicomponent \
		su/main/noise su/main/operations su/main/picking \
		su/main/stacking su/main/statics \
		su/main/stretching_moveout_resamp su/main/supromax \
		su/main/synthetics_waveforms_testpatterns su/main/tapering \
		su/main/transforms su/main/velocity_analysis su/main/well_logs \
		su/main/windowing_sorting_muting \
                su/graphics/psplot su/graphics/xplot tri/main \
                tri/graphics/psplot "

	EXISTS=no
	# loop through file extension types 
	for i in $SHELLS $LIBS $MAINS
	do
		EXTENSION=`echo $i | sed 's/\//\./g'`
		if [ -f "$STRIP/$NAME.$EXTENSION" ]
		then
			echo "In $CWPROOT/src/$i: "
			$PAGE_PROGRAM ${STRIP}/$NAME.$EXTENSION
			EXISTS=yes
		fi
	done

	if [ "$EXISTS" = "no" ]
	then 
		echo "There is no entry in the docs for \"$1\"" 2>&1 ; exit 1
	fi
;;
*) # echo usage message
	echo "Usage:  sudoc program_name"
esac

exit 0

