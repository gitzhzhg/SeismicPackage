#! /bin/sh
# Clean.sh - recursively call Clean.sh in the demo directories
# Test back-up dir first (at CWP must run on NeXT-net)


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
			Clean.sh
		fi
		rm -f *~ \#*\# .nfs* *.eps
		cd ..
	fi
done

# Take care of top directory
rm -f *~ \#*\# .nfs* *.eps

exit 0

