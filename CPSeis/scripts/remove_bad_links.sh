#!/bin/bash
# find bad links and remove them
HERE=$(pwd)
trap "exitnow" 1 2 3 4 5 6 9 11 12 15
exitnow() {
        cd $HERE
        exit 1
}
dospaces() {
        I=0
        while [ "$((I<DEPTH))" == 1 ] ; do
                echo -n " "
                I=$((I+1))
        done
}

findbadlink() {
        if [ -h $file ] ; then
                link=$(ls -l $file | awk '{print $NF}')
                if [ ! -e $link ] ; then
                        echo $dir
                        dospaces && ls $link
                        rm $file
                fi
        fi
}

removelinks() {
        for file in $(ls) ; do
                findbadlink
                if [ -d $file ] ; then
                        dospaces
                        #echo "$file is a dir"
                        DEPTH=$((DEPTH+1))
                        dir=$file
                        cd $dir
                        removelinks
                        cd ..
                        DEPTH=$((DEPTH-1))
                fi
        done
}
DEPTH=0
removelinks
echo ""
cd $HERE
