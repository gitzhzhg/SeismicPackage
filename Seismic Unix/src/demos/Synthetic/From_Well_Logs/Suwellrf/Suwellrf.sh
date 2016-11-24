#! /bin/sh


# convert depth velocity density from ascii to binary format
a2b outpar=nsamp.ascii n1=3 < sonic.txt > dvrfile.bin

# note that nsamp.ascii contains the value of 28306 samples
nval=28306

# compute reflectivity function with suwellrf
suwellrf ntr=50 nval=28306 dtout=.008 dvrfile=dvrfile.bin |
sufilter > data.su

# view data.su with suxwigb
suxwigb < data.su title="Reflectivity generated from welllog data" xcur=3 &

exit 0
