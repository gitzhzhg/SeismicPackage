#! /bin/ksh
# Create deconvolution test panels for an input cmp gather
# Authors: Jack, Ken
# NOTE: Comment lines preceeding user input start with  #!#
#set -x

#!# Set file etc.
input=cdpby100
cdp=601  # pick a cdp -- also used for naming output files
fold=30
space=6  # 6 null traces between panels
tpow=1   # gain power before deconvolutions

#!# Determine range of decon parameters (use milliseconds to run loop)
integer len lenmin=140 lenmax=280
integer gap gapmin=8   gapmax=32
gapincrement=0 gapexpand=2 lenincrement=0 lenexpand=1
tmin=2.0 tmax=4.0	# auto-corr window
f1=8 f2=12 f3=45 f4=55	# bandpass frequency corners

integer panelnumber=0	# test panel number


### Determine ns, nf and dt from data (for sunull)
nt=$(sugethw ns <$input | sed 1q | sed 's/.*ns=//')
dt=$(sugethw dt <$input | sed 1q | sed 's/.*dt=//')

### Convert dt to seconds from header value in microseconds
dt=$(bc -l <<-END
	scale=3
	$dt / 1000000
END)

### Label output according to cdp number and get the cdp
deconpanel=decon.$cdp
deconparams=deconparams.$cdp
suwind <$input key=cdp min=$cdp max=$cdp count=$fold >cdp.$cdp


# Loop over deconvolution parameters
>$deconpanel  # zero output files

# Zeroth panels are w/o decon
sugain <cdp.$cdp tpow=$tpow |
sufilter f=$f1,$f2,$f3,$f4 >>$deconpanel  
sunull ntr=$space nt=$nt dt=$dt >>$deconpanel

print "Test minlag maxlag tmin tmax" >$deconparams
gap=$gapmin
len=$lenmin
while ((gap <= gapmax && len <= lenmax))
do
	panelnumber=panelnumber+1
	minlag=$(bc -l <<-END
		scale=5; $gap / 1000
	END)
	maxlag=$(bc -l <<-END
		scale=5; $minlag + $len / 1000
	END)

	sugain <cdp.$cdp tpow=$tpow |
	supef minlag=$minlag maxlag=$maxlag  \
		mincorr=$tmin maxcorr=$tmax |
	sufilter f=$f1,$f2,$f3,$f4 >>$deconpanel  
	sunull ntr=$space nt=$nt dt=$dt >>$deconpanel

	print "$panelnumber   $minlag $maxlag $tmin $tmax" >>$deconparams
	gap=$(bc -l <<-END
		scale=5
		$gap*$gapexpand + $gapincrement
	END)
	len=$(bc -l <<-END
		scale=5
		$len*$lenexpand + $lenincrement
	END)
done

print "The parameter values are recorded in $deconparams"


### Plot deconvolution test panels
f2=0
d2=$(bc -l <<-END
	scale=5; 1/($fold + $space)
END)

### Additional display gaining and plot
### Some might like gpow=.5 instead of pbal=1, etc.
sugain <$deconpanel tpow=1 pbal=1 |
suxwigb f2=$f2 d2=$d2 perc=99 wbox=900 \
	title="File: $input  Deconvolution Test"  \
	label1="Time (s)"  label2="Deconvolution Test Number" & 
