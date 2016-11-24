#! /bin/sh

for i in *
do
	if [ -d $i ]
	then
		cd $i
		Clean.sh
		cd ..
	fi
done

