#! /bin/sh
# shell for generating prestack synthetic data gathers 
set -x
output=data

# build velocity model using "unif2"
nz=51 dz=50 fz=.0  labelz="Depth (m)"
nx=81 dx=50 fx=0.0  labelx="Distance (m)"
ninf=0 npmax=201 
unif2 <input >vfile1  ninf=$ninf  npmax=$npmax \
	nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
	v00=2000 

# build synthetic seismograms with "sysnvxz"
nt=501 dt=0.004 ft=0.0 tmin=0.2 nxm=201 dxm=15 fxm=500 dxo=200 nxo=5
fpeak=30 nx=81 nz=51 dx=50 dz=50 fx=-0 ls=1 nxb=50 nxd=5 fxo=100 

 susynvxz<vfile1  nt=$nt dt=$dt ft=$ft nxm=$nxm dxm=$dxm fxm=$fxm  fxo=$fxo\
      dxo=$dxo nxo=$nxo tmin=$tmin nx=$nx nz=$nz fx=$fx dx=$dx dz=$dz \
      nxb=$nxb nxd=$nxd fpeak=$fpeak  ls=$ls \
 	ref="1:0,500;4000,500" >$output

exit 0
