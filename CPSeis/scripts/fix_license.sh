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
        if ( head -2 $file | tail -1 | grep "<license>" >/dev/null ) ; then
                fix_license.py $file
                if [ -f $file.new ] ; then
                        if ( ! diff $file.new $file >/dev/null ) ; then
                                mv $file.new $file
                        else
                                rm $file.new
                        fi
                fi
        fi
}
rem_cpy () {
        if [ -d $file ] ; then
                dir=$file
                cd $dir
                echo "inside $dir"
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
                rem_cpy
        done
}

do_all
