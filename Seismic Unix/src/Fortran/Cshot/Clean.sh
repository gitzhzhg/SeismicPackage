#! /bin/sh
# Master Clean

for i in Demo* 
do
	cd $i
	Clean.sh
	cd ..
done
