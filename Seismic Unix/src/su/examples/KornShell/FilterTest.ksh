#! /bin/ksh
# Create filter panels for an input cmp gather
# Authors: Jack, Ken
# NOTE: Comment lines preceeding user input start with  #!#
#set -x

#!# Set file etc.
input=cdpby100
cdp=601		# pick a cdp -- also used for naming output files
fold=30
space=6		# 6 null traces between panels

#!# Determine range of cutoff frequencies
integer highmin=12 highmax=60 high
low=3
lowincrement=0 highincrement=8 lowexpand=1.1 highexpand=1
f1ratio=0.9 f2ratio=1.1 f3ratio=0.9 f4ratio=1.1 # for sufilter freqs
integer panelnumber=0  # test panel number (0 = unfiltered data)


### Determine ns and dt from data (for sunull)
nt=$(sugethw ns <$input | sed 1q | sed 's/.*ns=//')
dt=$(sugethw dt <$input | sed 1q | sed 's/.*dt=//')

### Convert dt to seconds from header value in microseconds
dt=$(bc -l <<-END
	scale=3
	$dt / 1000000
END)


### Label output according to cdp number and get the cdp
filpanel=fil.$cdp
filparams=filparams.$cdp
suwind <$input key=cdp min=$cdp max=$cdp count=$fold >cdp.$cdp



### Loop over frequencies
>$filpanel  # zero output file

cp cdp.$cdp $filpanel	# first (i.e. zeroth) panel is w/o any filter
sunull ntr=$space nt=$nt dt=$dt >>$filpanel

print "Test	flow	fhigh" >$filparams
high=highmin
while ((high<=highmax))
do
	panelnumber=panelnumber+1
	f1=$(bc -l <<-END
		scale=1;
		$f1ratio * $low
	END)
	f2=$(bc -l <<-END
		scale=1;
		$f2ratio * $low
	END)
	f3=$(bc -l <<-END
		scale=1;
		$f3ratio * $high
	END)
	f4=$(bc -l <<-END
		scale=1;
		$f4ratio * $high
	END)

	sufilter <cdp.$cdp f=$f1,$f2,$f3,$f4 >>$filpanel
	sunull ntr=$space nt=$nt dt=$dt >>$filpanel

	print "$panelnumber	$low	$high" >>$filparams
	low=$(bc -l <<-END
		scale=1;
		$low*$lowexpand + $lowincrement
	END)
	high=$(bc -l <<-END
		scale=1;
		$high*$highexpand + $highincrement
	END)
done

print "The parameter values are recorded in $filparams"


### Plot filter panels
f2=0
d2=$(bc -l <<-END
	1/($fold + $space)
END)

sugain <$filpanel tpow=2.0 gpow=.5 |
suxwigb f2=$f2 d2=$d2 perc=98  \
	wbox=800 \
	title="File: $filpanel  Filter Test "  \
	label1="Time (s)"  label2="Filter Number" & 
