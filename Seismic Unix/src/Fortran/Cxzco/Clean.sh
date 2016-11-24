#! /bin/sh
# Clean - master clean for this directory and ALL the Demo subdirectories

for i in Demo*
do
	cd $i
	Clean
	cd ..
done
