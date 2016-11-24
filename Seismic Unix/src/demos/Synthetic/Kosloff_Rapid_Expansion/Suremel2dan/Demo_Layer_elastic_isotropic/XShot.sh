#!/bin/sh
# This demo shows the use of the REM modeling program SUREMEL2DAN
# in isotropic mode.

unif2 < struc.txt par=parfile1.p v00=2000.,4000. > vp
unif2 < struc.txt par=parfile1.p v00=1155.,2310. > vs
unif2 < struc.txt par=parfile1.p v00=2000.,2500. > dens

NX=`grep nx= parfile2.p | sed "s/nx=//"`
NZ=`grep nz= parfile2.p | sed "s/nz=//"`
DX=`grep dx= parfile2.p | sed "s/dx=//"`
DZ=`grep dz= parfile2.p | sed "s/dz=//"`

time suremel2dan par=parfile2.p verbose=1

suaddhead < vp ns=$NZ > vp.su

suximage < vp.su title="P wave velocity model" d1=$DZ d2=$DX hbox=400 \
  label1="Depth [m]" label2="Distance [m]" legend=1 units="velocity [m/s]" &

suximage < xsect_p_0.su perc=99 title="Pressure shot record" \
  label1="Time [s]" label2="Distance [m]" &

suximage < xsect_uz_3.su perc=99 title="U_z diplacement shot record" \
  label1="Time [s]" label2="Distance [m]" &

suxmovie < snap_uz_3.su title=%g loop=1 clip=2.e-6 label1="Depth [m]" \
  label2="Distance [m]" title="U_z snapshot: Frame %g" height=400 width=400 &
