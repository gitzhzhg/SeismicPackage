#! /bin/ksh
# First break picking
# Author: Jack
# NOTE: Comment lines preceeding user input start with  #!#
#set -x

#!# Set parameters
input=cdpby100
cdp=601		# pick a cdp -- also used for naming output files
dxcdp=13.36
integer timefrac=2	# use first  nt/timefrac samples
integer fold=30 panelsize=6
#integer fold=6 panelsize=6

### Label output according to cdp number and get the cdp
pickfile=fbpicks.$cdp
integer nt=$(sugethw ns <$input | sed 1q | sed 's/.*ns=//')
integer itmax=nt/timefrac

suwind <$input key=cdp min=$cdp max=$cdp count=$fold |
suwind itmax=$itmax >cdp.$cdp

### Do the picking
print "Pick first breaks by moving mouse and typing 's', type 'Q' when done"
integer trace=1
while ((trace <= fold))
do
	sushw <cdp.$cdp key=tracr a=1 b=1 |
	suwind key=tracr min=$trace count=$panelsize |
	suxwigb perc=99 xcur=0.25 nbpi=96 f2=$trace d2=1 wbox=1100 \
		label1="Time (sec)" label2="Velocity (m/sec)" \
		mpicks=mpicks.$trace

	trace=trace+panelsize
done

set +x

### Combine the individual picks
print "Editing pick files ..."
trace=1
while ((trace <= fold))
do
	cat mpicks.$trace
	trace=trace+panelsize
done |
awk '
	BEGIN	{dx='$dxcdp'}
		{print $1 "	" (NR-1)*dx}
' >$pickfile

a2b <$pickfile >temp outpar=picks.p

xgraph <temp par=picks.p nplot=1 \
	label1="Time (sec)" label2="Range (m)" \
	title="First Breaks" \
	grid1=solid grid2=solid \
	linecolor=2 style=seismic &

### Clean up
trace=1
while ((trace <= fold))
do
	rm mpicks.$trace
	trace=trace+panelsize
done
rm temp

print "The first breaks are recorded in $pickfile"
