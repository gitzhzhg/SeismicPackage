#! /bin/sh
# /*********************** self documentation **********************/
# UPDATEDOC - put self-docs in ../doc/Stripped and ../doc/Headers
#
# Usage: updatedoc  path
#
# Notes:
# Paths include: cwp/main cwp/lib cwp/shell par/main par/lib par/shell
#     xplot/main xplot/lib psplot/main psplot/lib psplot/shell
#     Xtcwp/main Xtcwp/lib Sfio/main 
#      su/main/amplitudes su/main/attributes_parameter_estimation
#      su/main/convolution_correlation /su/main/data_compression
#      su/main/data_conversion su/main/datuming su/main/decon_shaping
#      su/main/dip_moveout su/main/filters su/main/headers su/main/interp_extrap
#      su/main/migration_inversion su/main/multicomponent su/main/noise
#      su/main/operations su/main/picking su/main/stacking su/main/statics
#      su/main/stretching_moveout_resamp su/main/supromax 
#      su/main/synthetics_waveforms_testpatterns su/main/tapering
#      su/main/transforms su/main/velocity_analysis su/main/well_logs 
#      su/main/windowing_sorting_muting
#     su/lib su/shell su/graphics/psplot
#     su/graphics/xplot tri/main tri/lib xtri tri/graphics/psplot
#     tetra/lib tetra/main
#     comp/dct/lib comp/dct/main comp/dct/libutil comp/dwpt/1d/lib
#     comp/dwpt/1d/main comp/dwpt/2d/lib comp/dwpt/2d/main
#     
# Use: updatedocall to update full directory, use updatehead to
#      to update the master header file.
#
# This shell builds the database used by  suname and gendocs 
# /**************** end self doc ********************************/

# Based on an original shell script by Jack K. Cohen
#
# $Author: john $
# $Source: /usr/local/cwp/src/par/shell/RCS/updatedoc.sh,v $
# $Revision: 1.23 $ ; $Date: 2010/01/25 18:00:36 $
#set -x

cmd=`basename $0`

case	$# in
	1)
		# ok
	;;
	*)
		
		echo "Usage: $cmd path"
		echo
		echo "Paths include: cwp/main cwp/lib  par/main par/lib par/shell "
		echo " xplot/main xplot/lib psplot/main psplot/lib "
		echo " Xtcwp/main Xtcwp/lib su/main su/lib su/graphics/psplot"
		echo " su/graphics/xplot "
		echo "su/graphics/xplot tri/main tri/lib xtri \
			tri/graphics/psplot"
		echo " tetra/lib tetra/main "
		echo " Trielas/lib Trielas/main Trielas/graphics/psplot  "
		echo "comp/dct/lib comp/dct/main comp/dct/libutil \
			 comp/dwpt/1d/lib"
		echo "comp/dwpt/1d/main comp/dwpt/2d/lib comp/dwpt/2d/main"
		echo
		echo "Use: updatedocall to update the full doc directory" 
		echo "     updatehead to update the master header file" 2>&1 \
			; exit 1
		
	;;
esac


##################$############################################################
# test for CWPROOT
###############################################################################
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
BIN=${ROOT}/bin
PATH=/bin:/usr/bin:/usr/ucb:$BIN

SRCDIR=${SRC}/$1

if [ ! -d $SRCDIR  ]
then
	echo "Can't find directory $SRCDIR" 2>&1; exit 1
fi

DOC=${SRC}/doc
STRIP=${DOC}/Stripped
HEAD=${DOC}/Headers

NAME=`echo $1 | sed 's/\//\./g'`


echo " Updating the $NAME doc files  "
echo

# Clear out old stuff; remake $STRIP and $HEAD directories
rm -rf $DOC/*/*.${NAME}

# make directories if necessary
if [ ! -d $DOC ]
then
	mkdir $DOC
fi

if [ ! -d $HEAD ]
then
	mkdir $HEAD
fi

if [ ! -d $STRIP ]
then
	mkdir $STRIP
fi


for i in ${SRCDIR}/*.*
do
	prog=`basename $i .c`
	nametemp=${prog}.${NAME}
	name=`echo $nametemp | sed 's/\.sh\./\./g'`

	sed -n '/* self documentation */,/* end self doc */p' $i |
	tee $DOC/$name |
	sed '
	/\*\*\*\*\*/d
	s/^\/\*//
	s/^\///
	s/^ \///
	/^char \*sdoc/d
	s/\\\"/\"/g
	s/"[	 ]*"\,//
	s/",$//
	s/^"//
	s/ \*\///
	s/^ \*/\*/
	/NULL}\;/d
	' >$STRIP/$name

	# if 1st line is blank, delete it.
	LINE1=`cat $STRIP/$name | awk ' NR==1 { print $2 }'`
	if test "$LINE1" = ""
	then
		sed '1d' $STRIP/$name > $STRIP/$name.tmp
		mv $STRIP/$name.tmp $STRIP/$name
	fi

	# make headers
	sed 1q $STRIP/$name  | sed 's/^ //'  >>$HEAD/HEADERS.${NAME}

	# fix files where 'end self doc' appears twice
	# remove leading # or *
	cat $STRIP/$name | sed '/end self doc/,100d' | sed '
						s/^\#//
						s/^\*//' > $STRIP/${name}.tmp
	mv $STRIP/${name}.tmp  $STRIP/$name

	echo -n "."

	# remove unstripped versions
	rm $DOC/$name

done

echo
echo " Doc  ${NAME} files updated "

exit 0
