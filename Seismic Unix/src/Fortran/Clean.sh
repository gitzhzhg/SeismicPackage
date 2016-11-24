#! /bin/sh
# Master Clean

set -x

for i in `ls`
do
	if [  -d $i ]
	then
		cd $i
		Clean.sh
		cd ..
	fi
	
done
