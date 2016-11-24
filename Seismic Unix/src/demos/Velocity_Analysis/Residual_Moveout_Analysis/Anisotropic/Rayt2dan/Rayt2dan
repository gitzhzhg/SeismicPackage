#! /bin/sh
# Demo for P- and SV-wave raytracing in heterogeneous anisotropic (VTI) media 
# RAYT2DAN
# Debashish Sarkar, June 2003
set -x

# define parameter fields
nz=100 dz=20 fz=0  labelz="Depth (m)"
nx=100 dx=20 fx=0  labelx="Midpoint (m)"

# define output dimensions
nxo=100	dxo=20 fxo=0
nzo=100 dzo=20 fzo=0

# define output shot locations
nsx=1 dsx=1 fsx=1000

# define initial conditions
fa=-75 da=2 na=76 amax=75 nt=1000

dt=0.008
ninf=0 npmax=401
r1=0 r2=0

# Create parameter files for raytracing.
# Make VP0
unif2 < test >VP0  ninf=$ninf  npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=1500 dvdz=0.6 dvdx=0.2
smooth2 < VP0 r1=$r1 r2=$r2 n1=$nz n2=$nx > VP0sm

# Make epsilon
unif2 < test >epsilon ninf=$ninf npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=0.2 dvdz=0.0001 dvdx=-0.0001
smooth2 < epsilon r1=$r1 r2=$r2 n1=$nz n2=$nx > epsilonsm

# Make delta
unif2 < test >delta  ninf=$ninf  npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=0.1 dvdz=0.00002 dvdx=0.000001
smooth2 < delta r1=$r1 r2=$r2 n1=$nz n2=$nx > deltasm

# Make VS0
unif2 < test >VS0  ninf=$ninf  npmax=$npmax \
        nz=$nz dz=$dz fz=$fz nx=$nx dx=$dx fx=$fx \
        v00=750 dvdz=0.6 dvdx=0.2
smooth2 < VS0 r1=$r1 r2=$r2 n1=$nz n2=$nx > VS0sm

# Ray trace
rayt2dan VP0file=VP0sm epsilonfile=epsilonsm deltafile=deltasm \
tfile=ttime fxo=$fxo nxo=$nxo dxo=$dxo fzo=$fzo nzo=$nzo dzo=$dzo fsx=$fsx \
nsx=$nsx dsx=$dsx nt=$nt nx=$nx nz=$nz dx=$dx dz=$dz fx=$fx dt=$dt fa=$fa \
da=$da na=$na amax=$amax

# Ray trace
rayt2dan SV=1 VS0file=VS0sm VP0file=VP0sm epsilonfile=epsilonsm \
deltafile=deltasm \
tfile=ttimeS fxo=$fxo nxo=$nxo dxo=$dxo fzo=$fzo nzo=$nzo dzo=$dzo fsx=$fsx \
nsx=$nsx dsx=$dsx nt=$nt nx=$nx nz=$nz dx=$dx dz=$dz fx=$fx dt=$dt fa=$fa \
da=$da na=$na amax=$amax

# Display
ximage < VP0sm n1=100 legend=1 title="VP0" f1=$fz f2=$fx \
d1=$dz d2=$dx xbox=0 ybox=0 wbox=325 hbox=325 lwidth=8 label2=$labelx \
ly=20 label1=$labelz &

ximage < epsilonsm n1=100 legend=1 title="epsilon" d1=$dz d2=$dx \
f1=$fz f2=$fx xbox=340 ybox=0 wbox=325 hbox=325 lwidth=8 label2=$labelx \
ly=20 label1=$labelz &

ximage < deltasm n1=100 legend=1 title="delta" d1=$dz d2=$dx \
f1=$fz f2=$fx xbox=680 ybox=0 wbox=325 hbox=325 lwidth=8 ly=20 label2=$labelx \
label1=$labelz &

ximage < VS0sm n1=100 legend=1 title="VS0" f1=$fz f2=$fx \
d1=$dz d2=$dx xbox=0 ybox=360 wbox=325 hbox=325 lwidth=8 label2=$labelx \
ly=20 label1=$labelz &

ximage < ttime n1=100 legend=1 \
title="P-wave traveltime table for shotpoint=1000 m" \
f1=$fz f2=$fx d1=$dz d2=$dx clip=1 xbox=340 ybox=360 wbox=325 hbox=325 \
lwidth=8 ly=20 label2=$labelx label1=$labelz grid1=dash grid2=dash &

ximage < ttimeS n1=100 legend=1 \
title="SV-wave traveltime table for shotpoint=1000 m" \
f1=$fz f2=$fx d1=$dz d2=$dx clip=1 xbox=680 ybox=360 wbox=325 hbox=325 \
lwidth=8 ly=20 label2=$labelx label1=$labelz grid1=dash grid2=dash &

# press the `h' or the `r' key to see color plots of the above figures!!!

exit
