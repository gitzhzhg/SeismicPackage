#! /bin/ksh
# Plot (approx) common offset section
# Author: Jack
# NOTE: Comment lines preceeding user input start with  #!#
set -x

#!# From values in Cos shell
integer minoffset=291 # minimum offset
integer doffset=106 # delta offset (take smallest)
integer j=0	# j is the index of the offset range (0 is near offset)

### Plot--note that data is not unpacked to disk
section=cos.$j.pack
integer min=minoffset+j*doffset
suunpack2 <$section |
sugain tpow=2.0 gpow=0.5 |
suximage f2=201 d2=1
	label1="Time (sec)" title="Common Offset: offset approx $min" \
	perc=99 grid1=solid &
