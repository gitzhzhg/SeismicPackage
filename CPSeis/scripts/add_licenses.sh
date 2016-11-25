#!/bin/bash
# add licenses to source files
#
#
HERE=$(pwd)

exitnow() {
        cd $HERE
        exit 1
}

trap "exitnow" 1 2 3 4 5 6 9 11 12 15

test_and_add () {
        if (! grep "<license>" $file >/dev/null ) ; then
                /homes/3DGeo/bmenger/bin/add_license_new.py $file
                if [ -f $file.new ] ; then
                        mv $file.new $file
                fi
        fi
}
add_lic () {
        if [ -d $file ] ; then
                dir=$file
                echo "working in directory $dir"
                cd $dir
                do_all
                cd ..
        elif [ -h $file ] ; then
                echo "$file is a link" >/dev/null
        elif [ -f $file ] ; then
               test_and_add 
        else
                echo "$file is bad"
        fi
}

do_all () {
        for file in $(ls) ; do
                add_lic
        done
}

do_all
