#! /bin/sh
# Demo for P- and SV-wave raytracing in heterogeneous anisotropic (VTI) media 
# RAYT2DAN
# Debashish Sarkar, June 2003
set -x

# define parameter fields
nz=150 dz=20 fz=0  labelz="Depth (m)"
nx=600 dx=20 fx=0  labelx="Midpoint (m)"

# define output dimensions
nxo=100	dxo=100 fxo=0
nzo=60 dzo=50 fzo=0

# define output shot locations
nsx=30 dsx=100 fsx=1000

# define initial conditions
fa=-75 da=2 na=76 amax=75 nt=1000

dt=0.008
ninf=0 npmax=401
r1=0 r2=0

# Create parameter files for raytracing.
# Make VP0
unif2 < test >VP0  ninf=$ninf  npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=2000 dvdz=0.3 dvdx=0.1
smooth2 < VP0 r1=$r1 r2=$r2 n1=$nz n2=$nx > VP0sm

# Make epsilon
unif2 < test >epsilon ninf=$ninf npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=0.0
smooth2 < epsilon r1=$r1 r2=$r2 n1=$nz n2=$nx > epsilonsm

# Make delta
unif2 < test >delta  ninf=$ninf  npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=0.0
smooth2 < delta r1=$r1 r2=$r2 n1=$nz n2=$nx > deltasm

# Ray trace
rayt2dan VP0file=VP0sm epsilonfile=epsilonsm deltafile=deltasm \
tfile=ttime fxo=$fxo nxo=$nxo dxo=$dxo fzo=$fzo nzo=$nzo dzo=$dzo fsx=$fsx \
nsx=$nsx dsx=$dsx nt=$nt nx=$nx nz=$nz dx=$dx dz=$dz fx=$fx dt=$dt fa=$fa \
da=$da na=$na amax=$amax

exit
