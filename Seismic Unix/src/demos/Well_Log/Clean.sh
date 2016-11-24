#! /bin/sh
# Clean - recursively call Clean in the demo directories
# Test back-up dir first (at CWP must run on dobrin)


DEMODIR=$CWPROOT/src/demos

find . -name core -print -exec rm -f {} \;
find . -name \.nfs\* -print -exec rm -f {} \;

# Take care of subdirectories
for i in `ls`
do
	if
		[ -d $i ]
	then
		cd $i
		if
			[ -f Clean.sh ]
		then
			echo $i
			Clean.sh
		fi
		rm -f *~ \#*\# .nfs* *.eps
		cd $DEMODIR
	fi
done

# Take care of top directory
rm -f *~ \#*\# .nfs* *.eps

exit 0
