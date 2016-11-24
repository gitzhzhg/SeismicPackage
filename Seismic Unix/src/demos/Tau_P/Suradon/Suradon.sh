#! /bin/sh


echo "preparing input cdp gather for SURADON"
sunhmospike | sufilter f=5,10,50,60 | \
	suxwigb title="input data fitting parabolic model, horz axis offset in m" &

echo "computing forward parabolic transform with SURADON"
sunhmospike | sufilter f=5,10,50,60 | suradon  | \
	suxwigb \
	title="forward parabolic transform, horz axis in ms of moveout on 2000 m offset" &

echo "computing estimate of multiples with SURADON"
sunhmospike | sufilter f=5,10,50,60 | \
	suradon choose=2 prewhite=0.01 pmula=40 pmulb=100 | \
	suxwigb title="estimate of multiples, horz axis offset in m" &

echo "computing data minus multiples with SURADON"
sunhmospike | sufilter f=5,10,50,60 | \
	suradon choose=1 prewhite=0.01 pmula=40 pmulb=100 | \
	suxwigb title="data minus multiples, horz axis offset in m" &

echo "computing forward and reverse transform to check transform error"
sunhmospike | sufilter f=5,10,50,60 | \
	suradon choose=3 prewhite=0.01 | \
	suxwigb title="forward and reverse transform test, horz axis offset in m" &

echo "computing forward and reverse transforms separately to test inverse transform option"
sunhmospike | sufilter f=5,10,50,60 | \
	suradon choose=0 prewhite=0.01 | \
	suradon choose=4 | \
	suxwigb title="forward and reverse transform done separately, horz axis offset in m" &

exit 0
