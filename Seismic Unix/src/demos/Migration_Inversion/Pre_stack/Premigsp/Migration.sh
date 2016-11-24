#!/bin/sh

# This script demonstrates prestack PSPI migration of shot records from
# FD modeling. In the shot records the direct arrivals are muted and the 
# seismograms are filtered to get rid of low frequency content due to
# the tail of the 2D Green's function.
# Finally the shot records are prestack PSPI migrated where the cross-
# correlation is normalized by the incident wave field and then stacked.

WBOX=945
HBOX=360

NX=`grep nx parfile1.par | sed 's/nx=//'`
NZ=`grep nz parfile1.par | sed 's/nz=//'`
DX=`grep dx parfile1.par | sed 's/dx=//'`
DZ=`grep dz parfile1.par | sed 's/dz=//'`

NSHOTS=20                  # number of shots
FSHOT=600                  # first horizontal shot position
DSHOT=$((2000 / $NSHOTS))  # distance between shots

VEL=2200     # velocity used for muting direct arrival
T2=0.12      # mute time at shot position

XS=$FSHOT
ISHOT=1

ProgramName=sumigpresp

# remove upper 2 velocity grid lines corresponding to datum at 10 m depth
NZ1=`expr $NZ - 2`

# transpose the velocity grid
transp < tmp n1=$NZ1 > vel_tr

echo
echo "  starting migration ..."
$ProgramName < data.su nflag=1 nxo=$NX dx=$DX nz=$NZ1 dz=$DZ nxshot=$NSHOTS fmax=25. f3=40. f4=50. vfile=vel_tr verbose=0 > image.su

suximage < image.su title="stacked image from $ProgramName " \
         label1="Depth [m]" label2="Distance [m]" \
         d1=$DZ d2=$DX wbox=$WBOX hbox=$HBOX &

echo
echo "  migration done."

exit
