#! /bin/sh
# shell for uniformly sampling velocity from a layered model
set -v

nz=51 dz=50 fz=.0  labelz="Depth (m)"
nx=81 dx=50 fx=0.0  labelx="Distance (m)"
ninf=0 npmax=201 
unif2 <input >vfile  ninf=$ninf  npmax=$npmax \
	nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
	v00=1500 dvdz=.8 

exit 0

