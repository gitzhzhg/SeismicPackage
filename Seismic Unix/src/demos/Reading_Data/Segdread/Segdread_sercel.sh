#! /bin/sh


# SU must be installed with the XDR flag set
# Sercel test. 
# Note, the sercel.segd file has the values of FFFFFFF for the second
# trace. This is not a number (NaN), so the output through sunan is 
# performed.

# change sercel.segd to your filename
inputdata=sercel.segd
outfile=sercel_temp.su

segdread tape=$inputdata verbose=1 ptmax=2 buff=0 aux=1 use_stdio=1 |
sunan verbose=0 >  $outfile

surange < $outfile

suxwigb < $outfile title="Sercel Test" perc=95  &

supswigb < $outfile title="Sercel Test" perc=95 > sercel_test.eps

exit 0
