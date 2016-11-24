#! /bin/sh

vel=1400
dvel=100
rm timemovie.su depthmovie.su
while [ "$vel" -lt 5000 ]
do
	sustolt < seismic3.su cdpmin=1 cdpmax=2142 vmig=$vel dxcdp=12.5 > tmp.su
	cat tmp.su >> timemovie.su

	suttoz < tmp.su v=$vel dz=3 nz=1500 >> depthmovie.su

	echo $vel

	vel=`expr $vel + $dvel`

done



#suxmovie < movie.su perc=99 n1=1500 n2=2142 loop=1 &

exit 0
