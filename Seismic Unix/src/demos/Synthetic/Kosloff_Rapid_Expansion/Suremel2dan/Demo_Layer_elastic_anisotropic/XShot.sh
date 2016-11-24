#!/bin/sh
# This demo shows the use of the REM modeling program SUREMEL2DAN
# in anisotropic mode

unif2 < struc.txt par=parfile1.p v00=31.257e9,70.328e9 > c11
unif2 < struc.txt par=parfile1.p v00=3.399e9,7,648e9   > c13
unif2 < struc.txt par=parfile1.p v00=0.,0              > c15
unif2 < struc.txt par=parfile1.p v00=22.487e9,50.596e9 > c33
unif2 < struc.txt par=parfile1.p v00=0.,0.             > c35
unif2 < struc.txt par=parfile1.p v00=6.486e9,14.594e9  > c55
unif2 < struc.txt par=parfile1.p v00=2.075e3,2.075e3   > dens

NX=`grep nx= parfile2.p | sed "s/nx=//"`
NZ=`grep nz= parfile2.p | sed "s/nz=//"`
DX=`grep dx= parfile2.p | sed "s/dx=//"`
DZ=`grep dz= parfile2.p | sed "s/dz=//"`

time suremel2dan par=parfile2.p verbose=1

suaddhead < c11 ns=$NZ > c11.su

suximage < c11.su title="c11 model" d1=$DZ d2=$DX hbox=400 \
  label1="Depth [m]" label2="Distance [m]" legend=1 units="c11 [N/m^2]" &

suximage < xsect_p_0.su perc=99 title="Pressure shot record" \
  label1="Time [s]" label2="Distance [m]" &

suximage < xsect_uz_3.su perc=99 title="U_z diplacement shot record" \
  label1="Time [s]" label2="Distance [m]" &

suxmovie < snap_uz_3.su title=%g loop=1 clip=2.e-6 label1="Depth [m]" 
  label2="Distance [m]" title="U_z snapshot: Frame %g" height=400 width=400 &
