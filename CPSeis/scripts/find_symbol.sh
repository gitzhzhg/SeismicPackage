HERE=$(pwd)
SYMBOL=$1
if [ -z $1 ] ; then
        echo "$0: usage-> $0 symbol_to_find"
        exit 1
fi
SPWS_HOME=/homes/3DGeo/bmenger/spws
LIBS=$SPWS_HOME/lib/${CPSEIS_ARCH}
for file in $LIBS/*.a ; do 
       nm $file | grep " T " | grep -i $SYMBOL && echo $file
done
UTIL=$SPWS_HOME/util
for dir in $UTIL/* ; do
        echo $dir
        grep -i $SYMBOL $dir/*.*
done
OOP=$SPWS_HOME/oop
for dir in $OOP/* ; do
        echo $dir
        grep -i $SYMBOL $dir/*.*
done
exit
GP=/apps/fusiongeo/geopress/oop
for dir in $GP/* ; do
        echo $dir
        grep -i $SYMBOL $dir/*.*
done
exit
HM=/homes/3DGeo/bmenger/geopress_etc3
for file in $HM/*.* ; do
        grep -i $SYMBOL $file
done
