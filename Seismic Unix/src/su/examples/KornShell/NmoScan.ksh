#! /bin/ksh
# Range of nmo corrections for a set of cmp gathers
# Authors: Jack, Ken
# NOTE: Comment lines preceeding user input start with  #!#
set -x

#!# Set input/output file names and data parameters
input=cdpby100
nmodata=nmoscan
fold=30
space=3		# 3 null traces between panels

#!# Determine velocity sampling.
integer vmin=1500   vmax=3000   dv=500 v
integer cdpmin=401 cdpmax=700 dcdp=100 cdp
#integer vmin=1500   vmax=4000   dv=250 v
#integer cdpmin=201 cdpmax=800 dcdp=100 cdp


### Determine ns and dt from data (for sunull)
nt=$(sugethw ns <$input | sed 1q | sed 's/.*ns=//')
dt=$(sugethw dt <$input | sed 1q | sed 's/.*dt=//')
### Convert dt to seconds from header value in microseconds
dt=$(bc -l <<-END
	$dt / 1000000
END)

### Do the nmo scan
>$nmodata  # zero output file
cdp=cdpmin
while ((cdp <= cdpmax))
do
	v=vmin
	while ((v <= vmax))
	do
		suwind <$input key=cdp min=$cdp max=$cdp count=$fold |
		sunmo cdp=$cdp vnmo=$v tnmo=0.0 >>$nmodata
		sunull ntr=$space nt=$nt dt=$dt >>$nmodata
		v=v+dv
	done
	sunull ntr=$space nt=$nt dt=$dt >>$nmodata
	cdp=cdp+dcdp
done

### Plot the nmo scan
integer nv=\(vmax-vmin\)/dv+1
f2=$cdpmin
d2=$(bc -l <<-END
	$dcdp/($nv * ($fold + $space) + $space)
END)

sugain <$nmodata tpow=2.0 |
suximage perc=99 f2=$f2 d2=$d2 \
	title="File: $input  NMO Scans "  \
	label1="Time (s)"  label2="CDP" & 
