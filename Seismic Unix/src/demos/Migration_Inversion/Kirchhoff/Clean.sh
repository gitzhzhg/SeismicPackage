#! /bin/sh

for i in `ls `
do
	if [ -d $i ]
	then
		cd $i
		Clean.sh
		cd ..
	fi
done
