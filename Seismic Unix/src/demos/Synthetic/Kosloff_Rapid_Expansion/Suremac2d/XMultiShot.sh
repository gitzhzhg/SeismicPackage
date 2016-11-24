#!/bin/sh
# This demo shows the use of the REM modeling program SUREMAC2D.
# Here five shot records at different shot locations are computed.
# For each run the same set of Bessel coefficients can be used.
# Therefore it is generated only during the first modeling run
# and is stored on disk for later reuse.

OPFLAG=$(grep opflag= parfile2.p | sed "s/opflag=//")

if [ $OPFLAG -eq 0 ]; then
  unif2 < struc.txt par=parfile0.p > dens
fi
unif2 < struc.txt par=parfile1.p > vel

NX=$(grep nx= parfile2.p | sed "s/nx=//")
NZ=$(grep nz= parfile2.p | sed "s/nz=//")
DX=$(grep dx= parfile2.p | sed "s/dx=//")
DZ=$(grep dz= parfile2.p | sed "s/dz=//")

NSHOTS=5                   # number of shots
FSHOT=500                  # first horizontal shot position
DSHOT=$((2500/$NSHOTS))    # distance between shots

XS=$FSHOT
ISHOT=1
PREC=1

> sections.su

while [ $ISHOT -le $NSHOTS ]; do

  ISX=$(echo $XS $DX / p | dc)
  echo "=============================="
  echo "  Shot position ISX="$ISX
  echo "=============================="

  time suremac2d par=parfile2.p prec=$PREC isx=$ISX verbose=1
  if [ $? -ne 0 ]; then
    exit;
  fi

  cat sectx.su \
| sushw key=fldr a=$ISHOT >> sections.su

  ISHOT=$(($ISHOT + 1))
  XS=$(($XS + $DSHOT))
  if [ $PREC -eq 1 ]; then
    PREC=2
  fi
done

suaddhead < vel ns=$NZ > vel.su

suximage < vel.su title="Velocity model" d1=$DZ d2=$DX hbox=400 \
  label1="Depth [m]" label2="Distance [m]" legend=1 units="velocity [m/s]" &

suximage < sections.su perc=99 title="$NSHOTS shot records" label1="time [s]" &
