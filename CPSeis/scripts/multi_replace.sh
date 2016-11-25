#!/bin/bash
# replace strings in multiple files
trap "exitnow" 1 2 3 4 5 6 9 11 12 15
HERE=$(pwd)
exitnow() {
        cd $HERE
        exit 1
}

if [ -z "$3" ] ; then
        echo "Usage: $0 'from string' 'to string' 'extension' [-r]"
        echo "       where -r means recursively down directory tree"
        echo " to and from strings should be in double quotes, extension ..i.e. .H "
        exit 1
fi
if [ ! -z $4 ] ; then
        if [ "$4" == "-r" ] ; then
                RECURSE=true
        else
                RECURSE=false
        fi
fi
if [ "$1" == "$2" ] ; then
        echo "Nothing to do."
        exit 0
fi

FROM=$1
TO=$2
EXT=$3

replace() {
        cat $file | sed "s:${FROM}:${TO}:g" >$file.$$tmp
        if ( ! diff $file $file.$$tmp >/dev/null ) ; then 
                mv $file $file.$$ && mv $file.$$tmp $file
        else
                rm $file.$$tmp
        fi
}

doall() {
        for file in $(ls *${EXT}) ; do
                 if [ -d $file ] ; then
                        if [ "$RECURSE" == "true" ] ; then
                                dir=$file
                                cd $dir
                                doall
                                cd ..
                        fi
                else
                        replace
                fi
        done
}
doall
cd $HERE
exit 0
