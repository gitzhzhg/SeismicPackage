#! /bin/sh
# make directories for CWP/SU codes installation

B=$CWPROOT/bin
I=$CWPROOT/include
L=$CWPROOT/lib

mkdir $CWPROOT
mkdir $B $L $I
mkdir $I/Xtcwp $L/X11 $L/X11/app-defaults $I/Xmcwp
mkdir $I/Triangles
mkdir $I/Wpc
mkdir $I/MGL
mkdir $I/Reflect

exit 0
