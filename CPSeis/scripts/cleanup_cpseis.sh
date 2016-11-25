#!/bin/bash
# clean out all *.a *.mod *.o *.log... files
#
Exclude="*.a *.mod *.o *.log *.icps *.started *.submitted *.aborted *.rpt.* *.so *.class"
[ ! -z $1 ] && export CPSEIS_INSTALL_DIR=$1
echo "Preparing to clean up $CPSEIS_INSTALL_DIR by removing all binaries"
echo -n "Is this ok? [yes/no] "
read prompt
HERE=$(pwd)
DIRS="$dir/i686_* $dir/linuxi* $dir/linuxab* $dir/sol[67]* $dir/x86_64* $dir/i686* $dir/64*"
if [ "$prompt" = "yes" ] ; then
	echo "cleaning..."
	cd $CPSEIS_INSTALL_DIR
	rm -rf $Exclude
	cd $CPSEIS_INSTALL_DIR/spws_home
	for upper in programs oop util lib bin ; do
		cd $upper
		for dir in * ; do
			echo $upper/$dir
			rm -f $dir/*/$dir
			rm -rf $DIRS
		done
		cd ..
	done
	cd $CPSEIS_INSTALL_DIR
	rm -rf platforms
	cd scripts
	chmod 755 *
	#./create_directories $CPSEIS_ARCH
else
	echo "aborting the clean."
fi
cd $HERE
