#! /bin/sh
# /*********************** self documentation **********************/
# SUHELP - list the CWP/SU programs and shells
#
# Usage:   suhelp
#
# /**************** end self doc ********************************/

PAGE_PROGRAM=cat

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
BIN=$ROOT/bin
SRC=${CWPSRC}/src
CWP=$SRC/cwp
PAR=$SRC/par
SU=$SRC/su
TRI=$SRC/tri
TETRA=$SRC/tetra
XPLOT=$SRC/xplot
PSPLOT=$SRC/psplot
XTCWP=$SRC/Xtcwp
XMCWP=$SRC/Xmcwp
XTRI=$SRC/xtri
DCT=$SRC/comp/dct
DWPT1D=$SRC/comp/dwpt/1d
DWPT2D=$SRC/comp/dwpt/2d

PATH=${PATH}:${BIN}

echo 
echo
echo "CWP PROGRAMS: (no self-documentation)"
cd $CWP/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM
echo ""

echo "PAR PROGRAMS: (programs with self-documentation)"
cd $PAR/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM
echo ""

pause

echo "SU PROGRAMS: (self-documented programs for SU data )"
echo "  SU data is \"SEGY\" data run through \"segyclean\""

LIST=" amplitudes attributes_parameter_estimation convolution_correlation \
data_compression data_conversion datuming decon_shaping dip_moveout \
filters gocad headers interp_extrap migration_inversion multicomponent \
noise operations picking stacking statics stretching_moveout_resamp \
supromax synthetics_waveforms_testpatterns tapering transforms \
velocity_analysis well_logs windowing_sorting_muting"

for i in $LIST
do
	NEWNAME=`echo $i | sed 's/_/ /' | tr [a-z] [A-Z]`
	echo " "
	echo "        SU PROGRAMS FOR $NEWNAME:"
	echo " "

	cd $SU/main/$i
	ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM
	echo ""
	pause
done

echo
echo "Delaunay Triangulation Materials:"
cd $TRI/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "Tetrahedra Materials:"
cd $TETRA/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "X-windows GRAPHICS for Delaunay Triangulation:"
cd $XTRI
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

pause

echo
echo "Straight X-windows GRAPHICS:"
cd $XPLOT/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "X-Toolkit based X-windows GRAPHICS:"
cd $XTCWP/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "Motif-based X-windows GRAPHICS:"
cd $XMCWP/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "X-windows GRAPHICS: for SU (\"segyclean\"-ed SEGY)  data sets"
cd $SU/graphics/xplot
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "PostScript GRAPHICS:"
cd $PSPLOT/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM
echo

echo
echo "PostScript GRAPHICS: for SU (\"segyclean\"-ed SEGY) data sets"
cd $SU/graphics/psplot
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

pause

echo
echo "Wavelet Packet Compression:"
cd $DWPT1D/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "Wavelet Packet Compression:"
cd $DWPT2D/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

echo
echo "2D Discrete Cosine Transform Compression:"
cd $DCT/main
ls -C *.c | sed 's/\.c/  /g' | $PAGE_PROGRAM

pause

echo
echo "CWP SHELLS SCRIPTS:"
cd $CWP/shell
ls -C *.sh | sed 's/\.sh/   /g' | $PAGE_PROGRAM

echo
echo "PAR SHELLS SCRIPTS:"
cd $PAR/shell
ls -C *.sh | sed 's/\.sh/   /g' | $PAGE_PROGRAM

echo
echo "POSTSCRIPT RELATED SHELLS SCRIPTS:"
cd $PSPLOT/shell
ls -C *.sh | sed 's/\.sh/   /g' | $PAGE_PROGRAM

echo
echo "SU SHELLS SCRIPTS:"
cd $SU/shell
ls -C *.sh | sed 's/\.sh/   /g' | $PAGE_PROGRAM

pause

echo 
echo "Use: \"suname\" to list the name and a brief description of all"
echo "of the CWP codes."

echo 
echo "Use \"sufind\" to find programs by keyword/name fragment."

echo
echo "Use: \"gendocs\" to compile a LaTeX document listing all self-docs." 

echo
echo "Use: \"sukeyword\" to find the SEGY header field keyword definitions."

echo
echo "Type: \"program_name <CR>\" to view its self documentation"
echo

echo "Note: not all programs listed here have the self-documentation feature"
echo "Type:  \"sudoc  program_name\" to list information for these programs."

echo
echo "For answers to Frequently Asked Questions, see the contents of:"
echo " $CWPROOT/src/faq"
echo

