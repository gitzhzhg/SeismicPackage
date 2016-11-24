#! /bin/ksh
# Select small sets of cdps for testing processing parameters
# NOTE: Comment lines preceeding user input start with  #!#
# Author: Jack
set -x

#!# Set input file name and fold (if known, else adjust suwind line)
input=cdp201to800.pack		# assuming packed input
integer fold=30

#!# Set output files and cdp ranges
spacedcdps=cdpby100		# unpacked output
cdpfirst=201 dcdp=100		# parameters to select non-contiguous cdps
contigcdps=cdp371to380		# unpacked output
integer cdpmin=371 cdpmax=380	# parameters to select contiguous cdps


### Window off a panel of cdps that ranges over the data
>$spacedcdps		# zap any previous file
suwind <$input key=cdp s=$cdpfirst j=$dcdp |
suunpack2 >>$spacedcdps 

### Window off a contiguous panel of cdps
>$contigcdps		# zap any previous file
integer count=fold*(cdpmax-cdpmin+1)
suwind <$input key=cdp min=$cdpmin max=$cdpmax count=$count |
suunpack2 >$contigcdps
