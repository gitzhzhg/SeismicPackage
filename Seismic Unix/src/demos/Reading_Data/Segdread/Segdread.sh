#! /bin/sh


# SU must be installed with the XDR flag set
inputdata=3stomp_test.segd
outfile=temp.su

segdread tape=$inputdata verbose=1 ptmax=2 buff=0 aux=1 use_stdio=1 > $outfile


surange < $outfile

suxwigb < $outfile title="3 Stomp test (segd)" &

supswigb < $outfile title="3 Stomp test (segd)" > 3stomp_test.eps

exit 0
