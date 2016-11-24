#! /bin/sh
# /*********************** self documentation **********************/
# UPDATEHEAD - update ../doc/Headers/Headers.all
#
# Usage: updatehead
#
# Notes:
#      
# This file builds the database used by  suname 
# /**************** end self doc ********************************/


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
BIN=${CWPROOT}/bin
SRC=${CWPSRC}/src
DOC=${SRC}/doc
HEAD=${DOC}/Headers
HEADALL=${HEAD}/Headers.all


PATH=/bin:/sbin:/usr/bin:/usr/ucb:/usr/bsd:/usr/sbin:${BIN}

echo "Updating ${HEADALL}"

cd ${HEAD}

# remove old header file
rm -f $HEADALL

SHELLS="cwp/shell par/shell psplot/shell su/shell "

LIBS=" cwp/lib par/lib su/lib psplot/lib xplot/lib Xtcwp/lib Xmcwp/lib \
tri/lib cwputils comp/dct/lib comp/dct/lib comp/dwpt/1d/lib \
comp/dwpt/2d/lib"

MAINS="cwp/main par/main psplot/main xplot/main Xtcwp/main Xmcwp/main  \
su/graphics/psplot su/main/amplitudes su/main/attributes_parameter_estimation \
su/main/convolution_correlation su/main/data_compression \
su/main/data_conversion su/main/datuming su/main/decon_shaping \
su/main/dip_moveout su/main/filters su/main/headers su/main/interp_extrap \
su/main/migration_inversion su/main/multicomponent su/main/noise \
su/main/operations su/main/picking su/main/stacking su/main/statics \
su/main/stretching_moveout_resamp su/main/supromax \
su/main/synthetics_waveforms_testpatterns su/main/tapering su/main/transforms \
su/main/velocity_analysis su/main/well_logs su/main/windowing_sorting_muting \
su/graphics/psplot su/graphics/xplot tri/main \
xtri tri/graphics/psplot comp/dct/main comp/dwpt/1d/main comp/dwpt/2d/main"

# build master header file
echo >> $HEADALL
echo "Mains: " >> $HEADALL

for i in $MAINS
do
	echo >> $HEADALL
	echo "In CWPROOT/src/${i}:" >> $HEADALL
	NAME=`echo $i | sed 's/\//\./g'`
	cat ${HEAD}/HEADERS.$NAME >> $HEADALL
done

echo >> $HEADALL
echo "Shells: " >> $HEADALL

for i in $SHELLS
do
	echo >> $HEADALL
	echo "In CWPROOT/src/${i}:" >> $HEADALL
	NAME=`echo $i | sed 's/\//\./g'`
	cat ${HEAD}/HEADERS.$NAME >> $HEADALL
done

echo >> $HEADALL
echo "Libs: " >> $HEADALL

for i in $LIBS
do
	echo >> $HEADALL
	echo "In CWPROOT/src/${i}:" >> $HEADALL
	NAME=`echo $i | sed 's/\//\./g'`
	cat ${HEAD}/HEADERS.$NAME >> $HEADALL
done

# remove any zerolength files remaining in the doc directory
cd $DOC
find . -type f -size 0 -print -exec rm {} \;

exit 0
