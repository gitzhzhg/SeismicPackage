#!/bin/bash
# remove a file extension from a group of files
[ -z "$1" ] && echo "Missing directory to work in" && exit 1
[ -z "$2" ] && echo "Missing extension to remove" && exit 1
DIR="$1"
EXT="$2"
for file in ls $DIR/*.$EXT ; do
        echo $file
        BASE=$(basename $file .$EXT)
        echo "mv $file $DIR/$BASE"
        mv $file $DIR/$BASE
done
ls -lrt $DIR
