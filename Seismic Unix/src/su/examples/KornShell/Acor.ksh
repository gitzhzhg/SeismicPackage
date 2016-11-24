#! /bin/ksh
# auto-correlation
# Author: Dave
set -x

### set parameters
panel=cdp601to610
cdpmin=601
cdpmax=610
fold=30

d2=$(bc -l <<-END
	1/$fold
END)

### autocorrelate the selected cmp's after gaining by t
sugain <$panel tpow=1 |
suacor norm=1 |
suximage f2=$cdpmin d2=$d2 \
	label1="Time (sec)" title="Autocor: CMPs $cdpmin to $cdpmax" \
	perc=99 grid1=solid &
