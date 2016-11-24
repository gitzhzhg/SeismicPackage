#! /bin/ksh
# Velocity analyses for the cmp gathers
# Authors: Dave, Jack
# NOTE: Comment lines preceeding user input start with  #!#
#set -x

#!# Set parameters
velpanel=cdpby100
alldata=cdp201to800.pack        # if not packed, zap call to suunpack2 below
vpicks=stkvel.p
normpow=0
slowness=0
integer cdpmin=301
integer cdpmax=800
integer dcdp=200
#integer dcdp=100
integer ncdp=3  # window of cdps to nmo on each side of current one
integer min max count
integer fold=30

#!# Set velocity sampling.
nv=51 dv=30 fv=1500
f1=3 f2=6 f3=30 f4=36


### Get header info
nout=$(sugethw ns <$velpanel | sed 1q | sed 's/.*ns=//')
dt=$(sugethw dt <$velpanel | sed 1q | sed 's/.*dt=//')
dxout=$(bc -l <<-END
        $dt / 1000000
END)


### Do the velocity analyses.
#print "Pick velocities by moving mouse and typing 's', type 'Q' when done"
integer cdp=cdpmin
while ((cdp <= cdpmax))
do
        ok=false
        while [[ $ok = false ]]
        do
                print "Starting velocity analysis for cdp $cdp"
                suwind <$velpanel key=cdp min=$cdp max=$cdp count=$fold |
                sugain tpow=2 |
                sufilter f=$f1,$f2,$f3,$f4 |
                suvelan nv=$nv dv=$dv fv=$fv \
                                normpow=$normpow slowness=$slowness |
                suximage bclip=0.2 wclip=0.0 f2=$fv d2=$dv \
                        label1="Time (sec)" label2="Velocity (m/sec)" \
                        title="Velocity Scan for CMP $cdp" \
                        grid1=solid grid2=solid cmap=default \
                        mpicks=mpicks.$cdp

                sort <mpicks.$cdp -n |
                mkparfile string1=tnmo string2=vnmo >par.$cdp

                edit=false
                PS3='Edit velocity picks? (Select number) '
                select i in yes no
                do
                        case $i in
                        (yes) edit=true
                        break;;
                        (no)  edit=false
                        break;;
                        (*)   print 'Invalid number';;
                        esac
                done

                if [[ $edit = true ]]
                then
                        vi par.$cdp
                fi

                print "Putting up velocity function for cdp $cdp"
                sed <par.$cdp '
                        s/tnmo/xin/
                        s/vnmo/yin/
                ' >unisam.p
                unisam nout=$nout fxout=0.0 dxout=$dxout \
                        par=unisam.p method=spline |
                xgraph n=$nout nplot=1 d1=$dxout f1=0.0 \
                        label1="Time (sec)" label2="Velocity (m/sec)" \
                        title="Stacking Velocity Function: CMP $cdp" \
                        grid1=solid grid2=solid \
                        linecolor=2 style=seismic &

                pause

                look=false
                PS3='Look at nmo of nearby cdps? (Select number) '
                select i in yes no
                do
                        case $i in
                        (yes) look=true
                        break;;
                        (no)  look=false
                        break;;
                        (*)   print 'Invalid number';;
                        esac
                done

                if [[ $look = true ]]
                then
                        print "Putting up nmo of nearby cdps (takes time)"
                        min=cdp-ncdp max=cdp+ncdp count=fold*(max-min+1)
                        d2=$(bc -l <<-END
                                1/$fold
                        END)
                        suunpack2 <$alldata |
                        suwind key=cdp min=$min max=$max count=$count |
                        sunmo par=par.$cdp |
                        sugain tpow=2 gpow=0.5 |
                        suximage f2=$min d2=$d2 \
                        perc=99 grid1=solid label1="Time (sec)" \
                        title="NMO of CMPs $min to $max" &
                        pause continue \(may want to wait for nmo to come up\)
                fi

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
        cdp=cdp+dcdp
done

set +x


### Combine the individual picks into a composite sunmo par file
print "Editing pick files ..."
>$vpicks
print -n "cdp=" >>$vpicks
cdp=cdpmin
print -n "$cdp" >>$vpicks
cdp=cdp+dcdp
while ((cdp <= cdpmax))
do
        print -n ",$cdp" >>$vpicks
        cdp=cdp+dcdp
done
print >>$vpicks

cdp=cdpmin
while ((cdp <= cdpmax))
do
        cat par.$cdp >>$vpicks
        cdp=cdp+dcdp
done


print "sunmo par file: $vpicks is ready"


### Clean up
cdp=cdpmin
while ((cdp <= cdpmax))
do
        rm mpicks.$cdp par.$cdp
        cdp=cdp+dcdp
done
rm unisam.p
