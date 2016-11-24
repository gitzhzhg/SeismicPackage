#! /bin/sh
# shell for Kirchhoff modeling for linear v(x,z)
#set -x

nt=1000 dt=0.004 ft=0.0 tmin=0.0

fxs=1.5 dxs=0.05 nxs=40
fxo=0.0 dxo=0.05 nxo=40

fpeak=30 er=0 ls=0 

echo "Generating data"
echo "^^^^^^^^^^^^^"
susynlvfti smooth=1 nt=$nt dt=$dt ft=$ft nxs=$nxs dxs=$dxs fxs=$fxs fxo=$fxo \
dxo=$dxo nxo=$nxo tmin=$tmin v00=2.0 dvdx=0.2 dvdz=0.6 \
fpeak=$fpeak er=1 ls=$ls epsilon=0.3 delta=0.1 \
ref="2.0,1.0;4.0,1.0" > TEST.data

exit 0
