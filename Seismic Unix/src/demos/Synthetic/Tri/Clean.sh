#! /bin/sh

cd datadir 
rm *
cd ..


for i in * 
do
	if test -d $i
	then
		cd $i
		Clean.sh
		cd ..
	fi
done
exit 0


