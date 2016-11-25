#!/bin/bash
# replace links with actual files pointed to
if [ -z $1 ] ; then
        echo "first arg = directory name within spws_home"
        exit 1
fi
DDD=$1
if [ -z $2 ] ; then
        echo "second arg = flag to append to file names."
        exit 1
fi
MYTAG=$2

cd ${CPSEIS_INSTALL_DIR}/spws_home/$DDD
HERE=$(pwd)
echo "About to replace links in this tree ... "$HERE"."
echo "Hit ^C to exit now, hit return to continue."
read answer
echo "continuing..."

trap "exitnow" 1 2 3 4 5 6 9 11 12 15
exitnow () {
        cd $HERE
        echo ""
        echo "Aborting..."
        exit 1
}

dospaces() {
        I=0
        while [ "$((I<DEPTH))" == 1 ] ; do
                echo -n " "
                I=$((I+1))
        done
}

fixlink () {
        if [ -h $file ] ; then
                link=$(ls -l $file | awk '{print $NF}')
                if [ "$(echo $link | grep kruger)" != "" ] ; then
                        dospaces
                        echo $link" <--"$file
                        if [ "$(echo $file | grep $MYTAG)" == "" ] ; then
                                echo "mv $file $file.$MYTAG && cp $link $file"
                        else
                                echo "Do not move, $file $MYTAG"
                        fi
                fi
        fi
}

replace_links () {
for file in * ; do
        fixlink
        if [ -d $file ] ; then
                DEPTH=$((DEPTH+1))
                dir=$file
                cd $dir
                dospaces && echo $(pwd)
                replace_links
                cd ..
                DEPTH=$((DEPTH-1))
        fi
done
}
DEPTH=0
replace_links
cd $HERE
exit 0
