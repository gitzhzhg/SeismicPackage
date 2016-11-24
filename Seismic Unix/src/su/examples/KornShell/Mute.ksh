#! /bin/ksh
# Muting
# Authors: Jack
# NOTE: Comment lines preceeding user input start with  #!#
#set -x

#!# Set file etc.
input=cdpby100
mutepicks=mute.p
cdp=601  # gather to pick mutes from
fold=30

#!# Label output according to cdp number and get the cdp
suwind <$input key=cdp min=$cdp max=$cdp count=$fold >cdp.$cdp

#!# Set offset info
f2=$(sugethw <cdp.$cdp offset | sed 1q | sed 's/.*offset=//')
d2=106.5

### Get header info
nout=$(sugethw ns <cdp.$cdp | sed 1q | sed 's/.*ns=//')
dt=$(sugethw dt <cdp.$cdp | sed 1q | sed 's/.*dt=//')
dxout=$(bc -l <<-END
	$dt / 1000000
END)

### Pick the mute times
print "Pick mute times by moving mouse and typing 's', type 'Q' when done"
ok=false
while [[ $ok = false ]]
do
	sugain <cdp.$cdp tpow=2 |
	suxwigb f2=$f2 d2=$d2 \
		label1="Time (sec)" label2="Offset (m)" \
		title="CMP $cdp" \
		grid1=solid grid2=solid \
		mpicks=mpicks.$cdp

	sort <mpicks.$cdp -n |
	mkparfile string1=tmute string2=xmute >$mutepicks

	print "Putting up mute function for cdp $cdp"
	sed <$mutepicks '
		s/tmute/xin/
		s/xmute/yin/
	' >unisam.p
	unisam nout=$nout fxout=0.0 dxout=$dxout \
		par=unisam.p method=linear |
	xgraph n=$nout nplot=1 d1=$dxout f1=0.0 \
		label1="Time (sec)" label2="Offset (m)" \
		title="Muting Function: CMP $cdp" \
		grid1=solid grid2=solid \
		linecolor=2 style=seismic &

	PS3='Picks OK? (Select number) '
	select i in yes no
	do
		case $i in
		(yes) ok=true
		break;;
		(no)  ok=false
		break;;
		(*)   print 'Invalid number';;
		esac
	done

done

set +x

print "sumute par file: $mutepicks is ready"

print "putting up muted panel"
sumute <$input par=mute.p |
suximage label1="Time (sec)" label2="Offset (m)" \
		title="Muted data: $input" \
		bclip=0.2 wclip=0.0 &


### Clean up
rm mpicks.$cdp
rm unisam.p
