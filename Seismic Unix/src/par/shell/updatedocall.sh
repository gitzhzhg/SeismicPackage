#! /bin/sh
# /*********************** self documentation **********************/
# UPDATEDOCALL - put self-docs in ../doc/Stripped
#
# Usage: updatedocall  
#
# Note: this shell uses updatedoc to update the  database used by
#       suname and gendocs 
#
# /**************** end self doc ********************************/

# Copyright 1988 by Jack K. Cohen
#
# $Author: john $
# $Source: /usr/local/cwp/src/par/shell/RCS/updatedocall.sh,v $
# $Revision: 1.15 $ ; $Date: 2010/01/25 18:00:36 $
#set -x


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
CWPSRC=$CWPROOT/src
fi

cmd=`basename $0`

BIN=${CWPROOT}/bin
PATH=/bin:/usr/bin:/${BIN}

echo "Updating the total CWP doc directory."
echo "This wipes out the contents of the doc directory"
echo "and takes a few minutes to complete."
echo -n "      Do you wish to continue [y/n]?"
read RSP

case $RSP in
	y*|Y*) # continue
		echo "Proceeding with update"
	;;
	*) # abort
		echo "Aborting update!" ; exit 1 
	;;
esac

echo "updating doc directory--- takes about 30 minutes"
echo "  .... please standby"
echo

DOC=${CWPSRC}/doc

# clear out all old docs
rm -rf $DOC/Stripped/*
rm -rf $DOC/TXT/*
rm -rf $DOC/Headers/*

SHELLS="cwp/shell par/shell psplot/shell su/shell "
LIBS1="psplot/lib xplot/lib Xtcwp/lib Xmcwp/lib "
LIBS2="su/lib cwputils tri/lib Trielas/lib tetra/lib comp/dct/lib comp/dct/libutil"
LIBS3="cwp/lib par/lib comp/dwpt/1d/lib comp/dwpt/2d/lib Mesa/lib "
MAINS1="cwp/main par/main psplot/main comp/dct/main comp/dwpt/1d/main Sfio/main"
MAINS2="xplot/main Xtcwp/main Xmcwp/main su/graphics/psplot su/graphics/xplot "
MAINS3="tri/main tri/graphics/psplot tetra/main xtri Trielas/main Trielas/graphics/psplot comp/dwpt/2d/main Mesa/main"
MAINS4="su/main/amplitudes su/main/attributes_parameter_estimation su/main/convolution_correlation su/main/data_compression su/main/data_conversion su/main/datuming su/main/decon_shaping su/main/dip_moveout su/main/filters su/main/headers su/main/interp_extrap su/main/migration_inversion su/main/multicomponent su/main/noise su/main/operations su/main/picking su/main/stacking su/main/statics su/main/stretching_moveout_resamp su/main/supromax su/main/synthetics_waveforms_testpatterns su/main/tapering su/main/transforms su/main/velocity_analysis su/main/well_logs  su/main/windowing_sorting_muting "

MAINS5="3D/Suinvco3d "

# do shells
for i in $SHELLS
do
	updatedoc $i &
done

# do libs
for i in $LIBS2
do
	updatedoc $i
done

for i in $LIBS1
do
	updatedoc $i &
done

for i in  $LIBS3
do
	updatedoc $i
done

for i in $MAINS1
do
	updatedoc $i
done

for i in $MAINS2
do
	updatedoc $i &
done

for i in $MAINS3 $MAINS4 $MAINS5
do
	updatedoc $i 
done

# update master header file
updatehead
echo "... doc database updated "

exit 0
